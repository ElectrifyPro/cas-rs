mod interval;

use std::{cell::RefCell, rc::{Rc, Weak}};

use cairo::{Context, Rectangle, TextExtents};
use crate::{CanvasPoint, GraphOptions, Point};
use interval::{IntervalSet, Interval};

/// Given a [`LabelerPoint`] and a [`Ray`] originating from it (decomposed into the first three
/// arguments), computes the space along the ray where the [`LabelerPoint`]'s label **would
/// intersect** the given rectangle (fourth argument).
///
/// The given rectangle's `x` and `y` coordinates are assumed to be the **center** of the
/// rectangle, with the width and height being the full width and height of the rectangle.
///
/// Note that the return value is an [`Interval`], not an [`IntervalSet`]. The intersection of two
/// rectangles along the ray can only occur once, so there is no need to represent multiple
/// intersections.
///
/// An empty interval indicates that the label doesn't intersect with the rectangle anywhere, and
/// the label can be placed freely along the ray.
///
/// See [`Ray`] for more information on what "free space" means.
///
/// This corresponds to the `LabelRectangleIntersection` function in the paper.
fn label_rectangle_intersection(
    origin: CanvasPoint<f64>,
    label_size: (f64, f64),
    ray_direction: CanvasPoint<f64>,
    rect: Rectangle,
) -> Interval {
    // there is one interval where the label can potentially intersect the rectangle
    let x_interval = {
        if ray_direction.0 == 0.0 {
            // no x-component, but an intersection can still occur
            // if the two rectangles overlap, the intersection interval is the entire positive real
            // number line, since no amount of translation along the ray can prevent the label from
            // intersecting the rectangle
            if rect.x() - rect.width() / 2.0 <= origin.0 + label_size.0 / 2.0
                && rect.x() + rect.width() / 2.0 >= origin.0 - label_size.0 / 2.0
            {
                Interval::positive()
            } else {
                Interval::empty()
            }
        } else {
            // # of `ray_direction.0` units to move label, so that its right edge touches the left edge of the rectangle
            let a = ((rect.x() - rect.width() / 2.0) - (origin.0 + label_size.0 / 2.0)) / ray_direction.0;

            // # of `ray_direction.0` units to move label, so that its left edge touches the right edge of the rectangle
            let b = ((rect.x() + rect.width() / 2.0) - (origin.0 - label_size.0 / 2.0)) / ray_direction.0;

            if ray_direction.0 > 0.0 {
                Interval::new(a, b)
            } else {
                Interval::new(b, a)
            }
        }
    };

    let y_interval = {
        if ray_direction.1 == 0.0 {
            // no y-component, but an intersection can still occur
            // if the two rectangles overlap, the intersection interval is the entire positive real
            // number line, since no amount of translation along the ray can prevent the label from
            // intersecting the rectangle
            if rect.y() - rect.height() / 2.0 <= origin.1 + label_size.1 / 2.0
                && rect.y() + rect.height() / 2.0 >= origin.1 - label_size.1 / 2.0
            {
                Interval::positive()
            } else {
                Interval::empty()
            }
        } else {
            // # of `ray_direction.1` units to move label, so that its top edge touches the bottom edge of the rectangle
            let c = ((rect.y() - rect.height() / 2.0) - (origin.1 + label_size.1 / 2.0)) / ray_direction.1;

            // # of `ray_direction.1` units to move label, so that its bottom edge touches the top edge of the rectangle
            let d = ((rect.y() + rect.height() / 2.0) - (origin.1 - label_size.1 / 2.0)) / ray_direction.1;

            if ray_direction.1 > 0.0 {
                Interval::new(c, d)
            } else {
                Interval::new(d, c)
            }
        }
    };

    Interval::positive()
        .intersection(x_interval)
        .intersection(y_interval)
}

/// Given a [`LabelerPoint`] and a [`Ray`] originating from it (decomposed into the first three
/// arguments), computes the space along the ray that is **blocked** by the given rectangle.
///
/// The given rectangle's `x` and `y` coordinates are assumed to be the **center** of the
/// rectangle, with the width and height being the full width and height of the rectangle.
///
/// Sometimes, a [`Ray`] for a [`LabelerPoint`] can end up totally blocked by another
/// [`LabelerPoint`]. In this case, the hypothetical label for the [`LabelerPoint`] cannot be
/// placed further than the blocking point, as the leader line for the label would intersect with
/// the blocking point. This function computes where this blocking occurs, if it does.
///
/// See [`Ray`] for more information on what "free space" means.
fn ray_rectangle_intersection(
    origin: CanvasPoint<f64>,
    ray_direction: CanvasPoint<f64>,
    rect: Rectangle,
) -> Interval {
    // we can defer to `label_rectangle_intersection` with the label extents set to a small value
    // representing the size of the ray, since the logic is the same

    // however, if an intersection is found, the intersection interval will go all the way to
    // infinity, since the leader line for the label would intersect the blocking rectangle forever
    // and ever
    let out = label_rectangle_intersection(origin, (1.0, 1.0), ray_direction, rect);
    if let Interval::Range(start, _) = out {
        Interval::new(start, f64::INFINITY)
    } else {
        Interval::empty()
    }
}

/// A ray originating from some [`LabelerPoint`], indicating a line with which the
/// [`LabelerPoint`]'s label can be placed on.
///
/// # Meaning of "free space"
///
/// A [`Ray`]'s `free_space` field contains the space along itself where a [`LabelerPoint`]'s label
/// can potentially be placed. This space is represented by a set of ranges (i.e. `[0, 4] U [8, ∞)`)
/// which denote a number of canvas units away from the [`LabelerPoint`]'s position that don't
/// intersect with any other obstruction on the canvas.
///
/// For example, if the free space of a ray is `[0, 4] U [8, ∞)`, then the label can be placed at
/// any point between 0 and 4 units away from the [`LabelerPoint`] along the ray, or 8 units and
/// beyond. Placing it at 5 units away would cause an intersection with some other obstruction.
#[derive(Debug)]
struct Ray {
    /// The parent [`LabelerPoint`] that this ray originates from.
    origin: Weak<RefCell<LabelerPoint>>,

    /// The direction of the ray, guaranteed to be a unit vector (of length 1.0).
    direction: CanvasPoint<f64>,

    /// The space along the ray where the label can be placed.
    free_space: IntervalSet,
}

impl Ray {
    /// Produces an arbitrary measure of the physical area covered by the interval set.
    ///
    /// See [`Interval::area`] for a detailed explanation of the area calculation.
    fn area(&self) -> f64 {
        self.free_space.area()
    }

    /// Creates a rectangle with the given width and height, translated the given number of units
    /// along the ray.
    fn translated_rect(&self, units: f64, width: f64, height: f64) -> Rectangle {
        let origin = self.origin.upgrade().unwrap();
        let origin = origin.borrow();
        Rectangle::new(
            origin.point_on_canvas.0 + units * self.direction.0,
            origin.point_on_canvas.1 + units * self.direction.1,
            width,
            height,
        )
    }

    /// Creates a point with the given width and height, translated the given number of units along
    /// the ray.
    fn translated_point(&self, units: f64) -> CanvasPoint<f64> {
        let origin = self.origin.upgrade().unwrap();
        let origin = origin.borrow();
        CanvasPoint(
            origin.point_on_canvas.0 + units * self.direction.0,
            origin.point_on_canvas.1 + units * self.direction.1,
        )
    }

    /// Computes and sets the ray's available space for the label at the start of the algorithm.
    fn compute_initial_free_space(
        &mut self,
        points: &[Rc<RefCell<LabelerPoint>>],
        canvas_size: CanvasPoint<u16>,
        padding: f64,
    ) {
        // begin with the entire positive real number line
        let mut free_space = IntervalSet::positive();

        let origin = self.origin.upgrade().unwrap();
        let (point, label_size) = {
            let origin_ref = origin.borrow();
            (origin_ref.point_on_canvas, origin_ref.label_size)
        };

        // remove space bounded by the canvas
        // do this by creating a rectangle around the canvas and computing the intersection
        let canvas_size = (canvas_size.0 as f64, canvas_size.1 as f64);
        let edges = [
            Rectangle::new(canvas_size.0 / 2.0, 0.0, canvas_size.0, 0.0), // top edge
            Rectangle::new(canvas_size.0, canvas_size.1 / 2.0, 0.0, canvas_size.1), // right edge
            Rectangle::new(canvas_size.0 / 2.0, canvas_size.1, canvas_size.0, 0.0), // bottom edge
            Rectangle::new(0.0, canvas_size.1 / 2.0, 0.0, canvas_size.1), // left edge
        ];
        for edge in edges {
            free_space.remove(ray_rectangle_intersection(
                point,
                self.direction,
                edge,
            ));
        }

        for obstacle_point in points {
            let obstacle_point_ref = obstacle_point.borrow();
            // NOTE: 20.0 = 10.0 (radius of point dot) * 2 (for diameter / width)
            let obstacle_point_rect = obstacle_point_ref.centered_rect(20.0 + padding, 20.0 + padding);

            if !self.origin.ptr_eq(&Rc::downgrade(obstacle_point)) {
                // remove space physically **blocked** by points
                // this is important for when a ray is totally blocked by a point; if this occurs,
                // we **cannot** place a label further than the blocking point, as any leader line
                // for our label would intersect with the blocking point
                // do this by creating a rectangle around the point and computing the intersection
                free_space.remove(ray_rectangle_intersection(
                    point,
                    self.direction,
                    obstacle_point_rect,
                ));
            }

            // remove space physically occupied by points, including origin point of ray (ignoring
            // the label as it's not placed yet)
            // do this by creating a rectangle around the point and computing the intersection
            free_space.remove(label_rectangle_intersection(
                point,
                label_size,
                self.direction,
                obstacle_point_rect,
            ));
        }

        self.free_space = free_space.clone();
    }
}

#[derive(Debug)]
pub struct LabelerPoint {
    point: Point<f64>,
    point_on_canvas: CanvasPoint<f64>,
    rays: Vec<Rc<RefCell<Ray>>>,

    /// The width and height of the label text.
    label_size: (f64, f64),

    /// The final computed position of the label, and the position to draw the leader line to.
    ///
    /// If [`None`] at the end of the algorithm, the label could not be placed.
    result: Option<(CanvasPoint<f64>, Option<(CanvasPoint<f64>, CanvasPoint<f64>)>)>,
}

impl LabelerPoint {
    /// Produces an arbitrary measure of the physical area covered by the interval set.
    ///
    /// See [`Interval::area`] for a detailed explanation of the area calculation.
    fn area(&self) -> f64 {
        self.rays.iter()
            .map(|ray| ray.borrow().area())
            .sum()
    }

    /// Creates a rectangle with the given width and height, centered at the point.
    fn centered_rect(&self, width: f64, height: f64) -> Rectangle {
        Rectangle::new(
            self.point_on_canvas.0,
            self.point_on_canvas.1,
            width,
            height,
        )
    }

    /// Returns the original point that this [`LabelerPoint`] is based on.
    pub fn point(&self) -> &Point<f64> {
        &self.point
    }

    /// Returns the point's canvas coordinates.
    pub fn point_on_canvas(&self) -> CanvasPoint<f64> {
        self.point_on_canvas
    }

    /// Returns the final computed position of the label and the position to draw the leader line to.
    pub fn result(&self) -> Option<(CanvasPoint<f64>, Option<(CanvasPoint<f64>, CanvasPoint<f64>)>)> {
        self.result
    }
}

/// Create `n` rays originating from the origin.
///
/// The first ray is the positive x-axis. Each successive ray is separated evenly, i.e. the angle
/// between each ray is `2π / n`.
fn create_base_rays(origin: &Rc<RefCell<LabelerPoint>>, n: usize) -> Vec<Rc<RefCell<Ray>>> {
    /// Attempts to remove small floating point errors from trigonometric calculations.
    fn round(angle: f64) -> f64 {
        (angle * 1e+15).round() / 1e+15
    }

    let angle_step = 2.0 * std::f64::consts::PI / n as f64;
    (0..n)
        .map(|i| {
            let angle = angle_step * i as f64;
            Rc::new(RefCell::new(Ray {
                origin: Rc::downgrade(origin),
                direction: CanvasPoint(round(angle.cos()), round(angle.sin())),
                free_space: IntervalSet::positive(),
            }))
        })
        .collect()
}

/// Wrapper struct for an algorithm used to label points on a graph while ensuring that as few point
/// labels overlap as possible.
///
/// The algorithms here are based on this paper:
///
/// [**An Efficient Algorithm for Scatter Chart Labeling**](https://www.think-cell.com/assets/en/career/talks/pdf/think-cell_article_aaai2006.pdf)
#[derive(Debug)]
pub struct Labeler {
    /// The points to find optimal label positions for.
    points: Vec<Rc<RefCell<LabelerPoint>>>,
}

impl Labeler {
    /// Initialize the labeler algorithm with the given points.
    pub fn new(
        ctxt: &Context,
        opts: GraphOptions,
        points: &[Point<f64>],
        rays_per_point: usize,
        padding: f64,
    ) -> Self {
        let labeler = Self {
            points: points.iter()
                .map(|point| {
                    let mut labeler_point = Rc::new(RefCell::new(LabelerPoint {
                        point: point.clone(),
                        point_on_canvas: opts.to_canvas(point.coordinates),
                        rays: Vec::new(),
                        label_size: {
                            let extents = if let Some(label) = &point.label {
                                ctxt.text_extents(label).unwrap()
                            } else {
                                ctxt.text_extents(&point.default_label()).unwrap()
                            };

                            (
                                extents.width() + extents.x_bearing() + padding,
                                extents.y_bearing().abs() + padding,
                            )
                        },
                        result: None,
                    }));
                    labeler_point.borrow_mut().rays = create_base_rays(&labeler_point, rays_per_point);
                    labeler_point
                })
                .collect(),
        };

        // compute the initial free space for each ray
        for point in &labeler.points {
            let point = point.borrow();
            for ray in &point.rays {
                ray.borrow_mut()
                    .compute_initial_free_space(&labeler.points, opts.canvas_size, padding);
            }
        }

        labeler
    }

    /// Chooses the best ray to place the next label on.
    ///
    /// This corresponds to the `FindBestRay` function in the paper.
    fn next_best_ray(&mut self) -> Option<Rc<RefCell<Ray>>> {
        // sort points by available space area in descending order
        // this places points with the most available space at the start

        // we want to prioritize points with little space by processing them later, since placing
        // labels around points that have more space could eliminate space from points with little
        // space
        self.points.sort_by(|p1, p2| {
            let p1 = p1.borrow();
            let p2 = p2.borrow();
            p2.area().partial_cmp(&p1.area()).unwrap()
        });

        struct Best {
            space: f64,
            ray: Rc<RefCell<Ray>>,
            set: Vec<f64>,
        }

        let mut best = None;

        let mut p_i = 0;
        while p_i < self.points.len() {
            let point = self.points[p_i].borrow();
            if point.result.is_some() {
                p_i += 1;
                continue;
            }

            let mut r_i = 0;
            'l2: while r_i < point.rays.len() {
                let ray = point.rays[r_i].borrow();
                let Some(units_away) = ray.free_space.start() else {
                    // label cannot be placed along this ray
                    r_i += 1;
                    continue;
                };
                let mut set = vec![];

                // for each ray, consider the label placed as close to the origin as possible
                // (choosing the start of this ray's free space area)
                // let potential_label = ray.translated_rect(units_away, point.label_extents.width(), point.label_extents.height());
                let potential_label = ray.translated_rect(units_away, point.label_size.0, point.label_size.1);

                // placing that label can potentially impact the space of all other points and rays
                let mut p_k = 0;
                while p_k < self.points.len() {
                    if p_i == p_k {
                        p_k += 1;
                        continue;
                    }

                    // for each other point `p_k`, we measure the amount of space it would have, if
                    // the above label were placed at the potential ray position

                    // this is done by taking each point `p_k`'s ray, removing the space occupied
                    // / removed by the hypothetically placed label, computing the ray's remaining
                    // space, and summing all the results

                    let point = self.points[p_k].borrow();
                    let p_k_remaining_space = point.rays
                        .iter()
                        .filter(|ray| !ray.borrow().free_space.is_empty())
                        .map(|ray| {
                            let ray = ray.borrow();
                            let mut free_space = ray.free_space.clone();
                            free_space.remove(label_rectangle_intersection(
                                point.point_on_canvas,
                                point.label_size,
                                ray.direction,
                                potential_label,
                            ));
                            free_space.remove(ray_rectangle_intersection(
                                point.point_on_canvas,
                                ray.direction,
                                potential_label,
                            ));
                            free_space.area()
                        })
                        .sum::<f64>();

                    if let Some(Best { space, .. }) = best {
                        if p_k_remaining_space < space {
                            r_i += 1;
                            continue 'l2;
                        }
                    }

                    // we determine the space left for all other points `p_k`
                    set.push(p_k_remaining_space);

                    p_k += 1;
                }

                set.sort_by(|a, b| a.partial_cmp(b).unwrap());

                // if no best ray has been chosen, or if the most recently computed set and ray is
                // better
                if best.as_ref().map_or(true, |best| set > best.set) { // TODO: flipped the comparison from paper
                    best = Some(Best {
                        space: set.iter()
                            .min_by(|a, b| a.partial_cmp(b).unwrap())
                            .copied()
                            // TODO: .unwrap_or() so that this works with one point
                            .unwrap_or(f64::INFINITY),
                        ray: point.rays[r_i].clone(),
                        set,
                    });
                }

                r_i += 1;
            }

            p_i += 1;
        }

        best.map(|best| best.ray)
    }

    /// Computes the best positions to place the labels for all the points.
    pub fn label_points(mut self) -> Vec<LabelerPoint> {
        // while there are still points with space
        while self.points.iter().any(|point| point.borrow().result.is_none()) {
            let Some(ray) = self.next_best_ray() else {
                // no more space for any label
                break;
            };
            let ray = ray.borrow();
            let point = ray.origin.upgrade().unwrap();
            let label_size = point.borrow().label_size;

            // place the label as close to the origin point as possible
            let Interval::Range(units_away, units_to_end) = ray.free_space.first() else {
                // should not happen, as we only choose rays with free space
                unreachable!();
            };
            let label_rect = ray.translated_rect(units_away, label_size.0, label_size.1);

            {
                // take the space from the point beyond; remove the space occupied by the label to
                // determine the location to attach the leader line to
                let mut ray_space = IntervalSet::new(Interval::Range(20.0, f64::INFINITY));
                ray_space.remove(ray_rectangle_intersection(
                    point.borrow().point_on_canvas,
                    ray.direction,
                    label_rect,
                ));

                point.borrow_mut().result = Some((
                    CanvasPoint(label_rect.x(), label_rect.y()),
                    ray_space.first().map(|start_units, end_units| {
                        (
                            ray.translated_point(start_units),
                            ray.translated_point(end_units),
                        )
                    }),
                ));
            }

            // update the free space for all rays
            for other_point in &self.points {
                if Rc::ptr_eq(&point, other_point) {
                    continue;
                }

                let other_point_ref = other_point.borrow();
                for other_ray in &other_point_ref.rays {
                    let mut other_ray = other_ray.borrow_mut();
                    let other_ray_direction = other_ray.direction;
                    other_ray.free_space.remove(label_rectangle_intersection(
                        other_point_ref.point_on_canvas,
                        other_point_ref.label_size,
                        other_ray_direction,
                        label_rect,
                    ));
                    other_ray.free_space.remove(ray_rectangle_intersection(
                        other_point_ref.point_on_canvas,
                        other_ray_direction,
                        label_rect,
                    ));
                }
            }
        }

        self.points.into_iter()
            .map(|point| Rc::into_inner(point).unwrap().into_inner())
            .collect()
    }
}
