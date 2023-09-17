mod eval;
pub mod opts;
pub mod point;

use cairo::{Context, Error, FontSlant, FontWeight, TextExtents};
use cas_parser::parser::expr::Expr;
use eval::evaluate_expr;
pub use point::{CanvasPoint, GraphPoint};
use rayon::prelude::*;
use super::text_align::ShowTextAlign;
pub use opts::GraphOptions;

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
        self.draw_grid_lines(context)?;
        self.draw_origin_axes(context, origin_canvas)?;

        let edges = self.draw_edge_labels(context, origin_canvas)?;
        self.draw_grid_line_numbers(context, origin_canvas, edges)?;

        self.draw_expressions(context)?;
        self.draw_points(context)?;

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
