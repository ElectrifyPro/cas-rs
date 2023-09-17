use cairo::{Context, Error, FontSlant, FontWeight, TextExtents};
use cas_eval::{ctxt::Ctxt, eval::Eval, value::Value};
use cas_parser::parser::expr::Expr;
use rayon::prelude::*;
use std::collections::HashMap;
use super::text_align::ShowTextAlign;

/// The extents of the edge labels.
struct EdgeExtents {
    /// The extents of the top edge label.
    pub top: TextExtents,

    /// The extents of the bottom edge label.
    pub bottom: TextExtents,

    /// The extents of the left edge label.
    pub left: TextExtents,

    /// The extents of the right edge label.
    pub right: TextExtents,
}

/// Round `n` to the nearest `k`.
fn round_to(n: f64, k: f64) -> f64 {
    (n / k).round() * k
}

/// Evaluate the given expression and returns the points to draw.
///
/// The options of the graph are used alongside the estimated derivative of the expression as an
/// optimization. If the evaluation of the expression results in a point outside the viewport, we
/// can start to assume that the next points might be outside the viewport as well. This allows us
/// to cut down on the number of points we need to evaluate.
///
/// Also in general, when the slope of the expression is steep, the step size can be somewhat
/// larger, since variation in the slope will not be as noticeable, in terms of someone observing
/// the graph.
///
/// However, when the slope of the expression is shallow, the step size must be smaller. For
/// example, if the step size is too large, we could end up skipping past a relative minimum /
/// maximum of the expression, which would be extremely obvious.
fn evaluate_expr(
    expr: &Expr,
    x_bounds: (f64, f64),
    options: GraphOptions,
) -> Vec<GraphPoint<f64>> {
    let mut ctxt = Ctxt::default();
    let mut points = Vec::new();

    let mut last_point = None;
    let mut current_point = None;

    let mut x = x_bounds.0;
    let min_step_len = options.scale.0 / 32.0;

    let y_bounds = (options.center.1 - options.scale.1, options.center.1 + options.scale.1);

    while x <= x_bounds.1 {
        ctxt.add_var("x", x.into());
        if let Ok(Value::Number(float)) = expr.eval(&mut ctxt).map(|v| v.coerce_real()) {
            let point = GraphPoint(x, float.to_f64());
            points.push(point);

            last_point = current_point;
            current_point = Some(point);
        }

        // adjust our step length based on the slope of the expression
        // NOTE: this would be so nice with let chains
        let step_len = if false && current_point.map(|p| p.1 < y_bounds.0 || p.1 > y_bounds.1).unwrap_or(false) {
            // if the expression moves outside the graph viewport, we hardcode the step length to
            // be an arbitrary high step length so we can get to a visible point more quickly
            min_step_len * 4.0
        } else if let (Some(last), Some(current)) = (last_point, current_point) {
            // in our slope calculation, we divide by the y-scale again to account for the scale
            // changing the visual slope of the expression
            //
            // for example, if we graph y=x^3 with a very large y-scale, an observer looking at the
            // graph will see a very shallow slope in the area around x=0, even though the true
            // slope of the expression past x=0 gets very steep very fast
            let slope = (current.1 - last.1) / (current.0 - last.0) / options.scale.1;

            // the larger the slope, the larger the step length can be
            // not too large though, so we can catch discontinuities
            min_step_len.max(slope.abs() / 10.0).min(min_step_len * 4.0)
        } else {
            min_step_len
        };
        x += step_len;
    }

    points
}

/// A pair of `(x, y)` values in **graph** units.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct GraphPoint<T>(pub T, pub T);

impl<T> From<(T, T)> for GraphPoint<T> {
    fn from((x, y): (T, T)) -> GraphPoint<T> {
        GraphPoint(x, y)
    }
}

/// A pair of `(x, y)` values in **canvas** units.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct CanvasPoint<T>(pub T, pub T);

/// Options to use when drawing a graph.
#[derive(Clone, Copy, Debug)]
pub struct GraphOptions {
    /// The width and height of the canvas, in pixels.
    pub canvas_size: CanvasPoint<u16>,

    /// The `(x, y)` point at which to center the graph.
    ///
    /// For example, to place the origin at the center of the graph, set this to `(0.0, 0.0)`.
    pub center: GraphPoint<f64>,

    /// The `(x, y)` scale of the graph.
    ///
    /// The scale indicates the distance, in graph units, from the center of the canvas to the edge
    /// of the canvas. For example, when the graph is centered at `(0.0, 0.0)` with a scale of
    /// `(10.0, 10.0)`, the visible graph will be from `(-10.0, -10.0)` to `(10.0, 10.0)`.
    pub scale: GraphPoint<f64>,

    /// The number of graph units between each minor grid line, given as a pair of `(x, y)` units.
    ///
    /// For example, to have a minor grid line every `3.0` units on the x-axis and every `2.0` units
    /// on the y-axis, set this to `(3.0, 2.0)`.
    pub minor_grid_spacing: GraphPoint<f64>,
}

impl Default for GraphOptions {
    fn default() -> GraphOptions {
        GraphOptions {
            canvas_size: CanvasPoint(1000, 1000),
            center: GraphPoint(0.0, 0.0),
            scale: GraphPoint(10.0, 10.0),
            minor_grid_spacing: GraphPoint(2.0, 2.0),
        }
    }
}

impl GraphOptions {
    /// Converts an x-value in **graph** space to an x-value in **canvas** space.
    fn x_to_canvas(&self, x: f64) -> f64 {
        let graph_space_range = self.scale.0 * 2.0;

        // normalize x-value to [0.0, 1.0], where 0.0 indicates left-edge of visible graph, 1.0
        // indicates right-edge of visible graph
        let normalized = (x - self.center.0) / graph_space_range + 0.5;

        // convert normalized x-value to canvas space
        normalized * self.canvas_size.0 as f64
    }

    /// Converts a y-value in **graph** space to a y-value in **canvas** space.
    fn y_to_canvas(&self, y: f64) -> f64 {
        let graph_space_range = self.scale.1 * 2.0;

        // normalize y-value to [0.0, 1.0], then flip the normalized value so, 0.0 is top, 1.0 is bottom
        // this is because the y-axis is flipped in graph space
        let normalized = 0.5 - (y - self.center.1) / graph_space_range;

        // convert normalized y-value to canvas space
        normalized * self.canvas_size.1 as f64
    }

    /// Converts a point in **graph** space to **canvas** space.
    pub fn to_canvas(&self, point: GraphPoint<f64>) -> CanvasPoint<f64> {
        CanvasPoint(
            self.x_to_canvas(point.0),
            self.y_to_canvas(point.1),
        )
    }

    /// Converts an x-value in **canvas** space to an x-value in **graph** space.
    fn x_to_graph(&self, x: f64) -> f64 {
        // normalize x-value to [0.0, 1.0], where 0.0 indicates left-edge of canvas, 1.0 indicates
        // right-edge of canvas (x should always be positive)
        let normalized = x / self.canvas_size.0 as f64;

        // convert normalized x-value to graph space
        let graph_space_range = self.scale.0 * 2.0;
        let left_edge_graph_space = self.center.0 - self.scale.0;

        normalized * graph_space_range + left_edge_graph_space
    }

    /// Converts a y-value in **canvas** space to a y-value in **graph** space.
    fn y_to_graph(&self, y: f64) -> f64 {
        // normalize y-value to [0.0, 1.0], then flip the normalized value so, 0.0 is bottom, 1.0 is top
        // this is because the y-axis is flipped in canvas space
        let normalized = 1.0 - y / self.canvas_size.1 as f64;

        // convert normalized y-value to graph space
        let graph_space_range = self.scale.1 * 2.0;
        let bottom_edge_graph_space = self.center.1 - self.scale.1;

        normalized * graph_space_range + bottom_edge_graph_space
    }

    /// Converts a point in **canvas** space to **graph** space.
    pub fn to_graph(&self, point: CanvasPoint<f64>) -> GraphPoint<f64> {
        GraphPoint(
            self.x_to_graph(point.0),
            self.y_to_graph(point.1),
        )
    }
}

/// A graph containing expressions to draw.
#[derive(Clone, Debug, Default)]
pub struct Graph {
    /// The expressions to draw.
    pub expressions: Vec<Expr>,

    /// The points to draw.
    pub points: Vec<GraphPoint<f64>>,

    /// The rendering options for the graph.
    pub options: GraphOptions,
}

impl Graph {
    /// Create a new, empty graph.
    pub fn new() -> Graph {
        Graph::default()
    }

    /// Create a new graph with the given options.
    pub fn with_opts(options: GraphOptions) -> Graph {
        Graph {
            options,
            ..Graph::default()
        }
    }

    /// Add an expression to the graph.
    pub fn add(&mut self, expr: Expr) {
        self.expressions.push(expr);
    }

    /// Center the graph on the points in the graph.
    pub fn center_on_points(&mut self) {
        let mut sum = GraphPoint(0.0, 0.0);

        // find the average of the points and center on that
        for point in self.points.iter() {
            sum.0 += point.0;
            sum.1 += point.1;
        }

        self.options.center = GraphPoint(
            sum.0 / self.points.len() as f64,
            sum.1 / self.points.len() as f64,
        );

        // find the point furthest from the center and scale so that is is visible
        let mut max_dist = 0.0;
        for point in self.points.iter() {
            let dist = (point.0 - self.options.center.0).hypot(point.1 - self.options.center.1);
            if dist > max_dist {
                max_dist = dist;
            }
        }
        self.options.scale = GraphPoint(
            max_dist * 1.5,
            max_dist * 1.5,
        );
    }

    /// Draw the graph to the given context.
    pub fn draw(&self, context: &Context) -> Result<(), Error> {
        context.set_source_rgb(0.0, 0.0, 0.0);
        context.paint()?;

        context.select_font_face("sans-serif", FontSlant::Oblique, FontWeight::Normal);

        let origin_canvas = self.options.to_canvas(GraphPoint(0.0, 0.0));
        self.draw_grid_lines(&context)?;
        self.draw_origin_axes(&context, origin_canvas)?;

        let edges = self.draw_edge_labels(&context, origin_canvas)?;
        self.draw_grid_line_numbers(&context, origin_canvas, edges)?;

        self.draw_expressions(&context)?;
        self.draw_points(&context)?;

        Ok(())
    }

    /// Draw minor grid lines.
    fn draw_grid_lines(
        &self,
        context: &Context,
    ) -> Result<(), Error> {
        context.set_source_rgb(0.4, 0.4, 0.4);
        context.set_line_width(2.0);

        // vertical grid lines (x = ...)
        let vert_bounds = (
            round_to(self.options.center.0 - self.options.scale.0, self.options.minor_grid_spacing.0),
            round_to(self.options.center.0 + self.options.scale.0, self.options.minor_grid_spacing.0),
        );
        let mut x = vert_bounds.0;
        while x <= vert_bounds.1 {
            // is this grid line within the canvas bounds?
            let x_canvas = self.options.x_to_canvas(x);
            if x_canvas < 0.0 || x_canvas > self.options.canvas_size.0 as f64 {
                x += self.options.minor_grid_spacing.0;
                continue;
            }

            context.move_to(x_canvas, 0.0);
            context.line_to(x_canvas, 1000.0);
            context.stroke()?;

            x += self.options.minor_grid_spacing.0;
        }

        // horizontal grid lines (y = ...)
        let hor_bounds = (
            round_to(self.options.center.1 - self.options.scale.1, self.options.minor_grid_spacing.1),
            round_to(self.options.center.1 + self.options.scale.1, self.options.minor_grid_spacing.1),
        );
        let mut y = hor_bounds.0;
        while y <= hor_bounds.1 {
            let y_canvas = self.options.y_to_canvas(y);
            if y_canvas < 0.0 || y_canvas > self.options.canvas_size.1 as f64 {
                y += self.options.minor_grid_spacing.1;
                continue;
            }

            context.move_to(0.0, y_canvas);
            context.line_to(1000.0, y_canvas);
            context.stroke()?;

            y += self.options.minor_grid_spacing.1;
        }

        Ok(())
    }

    /// Draw minor grid line numbers.
    fn draw_grid_line_numbers(
        &self,
        context: &Context,
        origin_canvas: CanvasPoint<f64>,
        edges: EdgeExtents,
    ) -> Result<(), Error> {
        // TODO: check collisions between grid line numbers themselves
        context.set_source_rgb(1.0, 1.0, 1.0);
        context.set_font_size(30.0);

        let padding = 10.0;

        // vertical grid line numbers
        let vert_bounds = (
            round_to(self.options.center.0 - self.options.scale.0, self.options.minor_grid_spacing.0),
            round_to(self.options.center.0 + self.options.scale.0, self.options.minor_grid_spacing.0),
        );
        let mut x = vert_bounds.0;
        while x <= vert_bounds.1 {
            // skip 0.0, as the origin axes will be drawn later
            if x == 0.0 {
                x += self.options.minor_grid_spacing.0;
                continue;
            }

            // is this grid line number within the canvas bounds?
            let x_canvas = self.options.x_to_canvas(x);
            if x_canvas < 0.0 || x_canvas > self.options.canvas_size.0 as f64 {
                x += self.options.minor_grid_spacing.0;
                continue;
            }

            let x_value_str_raw = format!("{:.3}", x);
            let x_value_str = x_value_str_raw.trim_end_matches('0').trim_end_matches('.');
            let x_value_extents = context.text_extents(x_value_str)?;

            // will this grid line number collide with the left / right edge labels?
            let text_left_bound = x_canvas - x_value_extents.width() / 2.0;
            let text_right_bound = x_canvas + x_value_extents.width() / 2.0;
            if text_left_bound < edges.left.width() + padding
                || text_right_bound > self.options.canvas_size.0 as f64 - edges.right.width() - padding {
                x += self.options.minor_grid_spacing.0;
                continue;
            }

            context.show_text_align_with_extents(
                x_value_str,
                (x_canvas, origin_canvas.1 + padding),
                (0.5, 1.0),
                &x_value_extents,
            )?;

            x += self.options.minor_grid_spacing.0;
        }

        // horizontal grid line numbers
        let hor_bounds = (
            round_to(self.options.center.1 - self.options.scale.1, self.options.minor_grid_spacing.1),
            round_to(self.options.center.1 + self.options.scale.1, self.options.minor_grid_spacing.1),
        );
        let mut y = hor_bounds.0;
        while y <= hor_bounds.1 {
            // same as above, but for the y-axis
            if y == 0.0 {
                y += self.options.minor_grid_spacing.1;
                continue;
            }

            let y_canvas = self.options.y_to_canvas(y);
            if y_canvas < 0.0 || y_canvas > self.options.canvas_size.1 as f64 {
                y += self.options.minor_grid_spacing.1;
                continue;
            }

            let y_value_str_raw = format!("{:.3}", y);
            let y_value_str = y_value_str_raw.trim_end_matches('0').trim_end_matches('.');
            let y_value_extents = context.text_extents(y_value_str)?;

            let text_top_bound = y_canvas - y_value_extents.height() / 2.0;
            let text_bottom_bound = y_canvas + y_value_extents.height() / 2.0;
            if text_top_bound < edges.top.height() + padding
                || text_bottom_bound > self.options.canvas_size.1 as f64 - edges.bottom.height() - padding {
                y += self.options.minor_grid_spacing.1;
                continue;
            }

            context.show_text_align_with_extents(
                y_value_str,
                (origin_canvas.0 + padding, y_canvas),
                (0.0, 0.5),
                &y_value_extents,
            )?;

            y += self.options.minor_grid_spacing.1;
        }

        Ok(())
    }

    /// Draw the origin axes if applicable.
    fn draw_origin_axes(
        &self,
        context: &Context,
        origin_canvas: CanvasPoint<f64>,
    ) -> Result<(), Error> {
        context.set_source_rgb(1.0, 1.0, 1.0);
        context.set_line_width(5.0);

        // vertical axis (x = 0)
        if origin_canvas.0 >= 0.0 && origin_canvas.0 <= self.options.canvas_size.0 as f64 {
            context.move_to(origin_canvas.0, 0.0);
            context.line_to(origin_canvas.0, 1000.0);
            context.stroke()?;
        }

        // horizontal axis (y = 0)
        if origin_canvas.1 >= 0.0 && origin_canvas.1 <= self.options.canvas_size.1 as f64 {
            context.move_to(0.0, origin_canvas.1);
            context.line_to(1000.0, origin_canvas.1);
            context.stroke()?;
        }

        Ok(())
    }

    /// Draw the edge labels (the values at the edge of the canvas).
    ///
    /// Returns the extents of each edge label, which is used to mask minor grid line numbers.
    fn draw_edge_labels(
        &self,
        context: &Context,
        origin_canvas: CanvasPoint<f64>,
    ) -> Result<EdgeExtents, Error> {
        context.set_source_rgb(1.0, 1.0, 1.0);
        context.set_font_size(40.0);

        let padding = 10.0;

        // top edge, bottom edge
        let x = origin_canvas.0 + padding;
        let top_value = format!("{:.3}", self.options.center.1 + self.options.scale.1);
        let top = context.show_text_align(
            top_value.trim_end_matches('0').trim_end_matches('.'),
            (x, padding),
            (0.0, 1.0),
        )?;

        let bottom_value = format!("{:.3}", self.options.center.1 - self.options.scale.1);
        let bottom = context.show_text_align(
            bottom_value.trim_end_matches('0').trim_end_matches('.'),
            (x, self.options.canvas_size.1 as f64 - padding),
            (0.0, 0.0),
        )?;

        // left edge, right edge
        let y = origin_canvas.1 + padding;
        let left_value = format!("{:.3}", self.options.center.0 - self.options.scale.0);
        let left = context.show_text_align(
            left_value.trim_end_matches('0').trim_end_matches('.'),
            (padding, y),
            (0.0, 1.0),
        )?;

        let right_value = format!("{:.3}", self.options.center.0 + self.options.scale.0);
        let right = context.show_text_align(
            right_value.trim_end_matches('0').trim_end_matches('.'),
            (self.options.canvas_size.0 as f64 - padding, y),
            (1.0, 1.0),
        )?;

        Ok(EdgeExtents { top, bottom, left, right })
    }

    /// Draw the expressions in the graph.
    fn draw_expressions(
        &self,
        context: &Context,
    ) -> Result<(), Error> {
        // evaluate expressions and draw as we go
        context.set_source_rgb(1.0, 0.0, 0.0);
        context.set_line_width(5.0);

        let x_bounds = (
            self.options.center.0 - self.options.scale.0,
            self.options.center.0 + self.options.scale.0,
        );

        let expr_points = self.expressions.par_iter()
            .map(|expr| evaluate_expr(expr, x_bounds, self.options))
            .collect::<Vec<_>>();
        for points in expr_points {
            let mut first_eval = true;
            for point in points {
                let canvas = self.options.to_canvas(point);
                if first_eval {
                    context.move_to(canvas.0, canvas.1);
                    first_eval = false;
                } else {
                    context.line_to(canvas.0, canvas.1);
                }
            }
            context.stroke()?;
        }

        Ok(())
    }

    /// Draw the points in the graph.
    fn draw_points(
        &self,
        context: &Context,
    ) -> Result<(), Error> {
        context.set_source_rgb(1.0, 0.0, 0.0);
        context.set_font_size(30.0);

        for point in self.points.iter() {
            let canvas = self.options.to_canvas(*point);
            context.arc(canvas.0, canvas.1, 10.0, 0.0, 2.0 * std::f64::consts::PI);
            context.fill()?;

            // draw the point's coordinates
            let point_value = (
                format!("{:.3}", point.0),
                format!("{:.3}", point.1),
            );
            context.show_text_align(
                &format!(
                    "({}, {})",
                    point_value.0.trim_end_matches('0').trim_end_matches('.'),
                    point_value.1.trim_end_matches('0').trim_end_matches('.')
                ),
                (canvas.0, canvas.1),
                (-0.1, -0.1),
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test the conversion functions from canvas to graph space.
    #[test]
    fn canvas_to_graph() {
        let graph = Graph::with_opts(GraphOptions {
            canvas_size: CanvasPoint(465, 917),
            center: GraphPoint(-3.0, 2.41),
            scale: GraphPoint(3.59, 5.69),
            minor_grid_spacing: GraphPoint(2.0, 2.0),
        });

        assert_eq!(
            graph.options.x_to_graph(0.0),
            graph.options.center.0 - graph.options.scale.0,
        );
        assert_eq!(
            graph.options.x_to_graph(graph.options.canvas_size.0 as f64),
            graph.options.center.0 + graph.options.scale.0,
        );
        assert_eq!(
            graph.options.y_to_graph(0.0),
            graph.options.center.1 + graph.options.scale.1,
        );
        assert_eq!(
            graph.options.y_to_graph(graph.options.canvas_size.1 as f64),
            graph.options.center.1 - graph.options.scale.1,
        );
    }

    /// Test the conversion functions from graph to canvas space.
    #[test]
    fn graph_to_canvas() {
        let graph = Graph::with_opts(GraphOptions {
            canvas_size: CanvasPoint(465, 917),
            center: GraphPoint(-3.0, 2.41),
            scale: GraphPoint(3.59, 5.69),
            minor_grid_spacing: GraphPoint(2.0, 2.0),
        });

        assert_eq!(
            graph.options.x_to_canvas(graph.options.center.0 - graph.options.scale.0),
            0.0,
        );
        assert_eq!(
            graph.options.x_to_canvas(graph.options.center.0 + graph.options.scale.0),
            graph.options.canvas_size.0 as f64,
        );
        // precision is wonky with this one
        assert_eq!(
            round_to(graph.options.y_to_canvas(graph.options.center.1 + graph.options.scale.1), 1e-6),
            0.0,
        );
        assert_eq!(
            graph.options.y_to_canvas(graph.options.center.1 - graph.options.scale.1),
            graph.options.canvas_size.1 as f64,
        );
    }
}
