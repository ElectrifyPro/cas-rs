use cairo::{Context, Error, FontSlant, FontWeight, TextExtents};
use cas_eval::{ctxt::Ctxt, eval::Eval, value::Value};
use cas_parser::parser::expr::Expr;
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

/// A pair of `(x, y)` values in **graph** units.
#[derive(Clone, Copy, Debug, Default)]
pub struct GraphPoint<T>(T, T);

/// A pair of `(x, y)` values in **canvas** units.
#[derive(Clone, Copy, Debug, Default)]
pub struct CanvasPoint<T>(T, T);

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
            center: GraphPoint(-3.0, 2.41),
            scale: GraphPoint(10.0, 10.0),
            minor_grid_spacing: GraphPoint(2.0, 2.0),
        }
    }
}

impl GraphOptions {
    /// Converts an x-value in **graph** space to an x-value in **canvas** space.
    fn x_to_canvas(&self, x: f64) -> f64 {
        let half_size = self.canvas_size.0 as f64 / 2.0;
        (x - self.center.0) * half_size / self.scale.0 + half_size
    }

    /// Converts a y-value in **graph** space to a y-value in **canvas** space.
    fn y_to_canvas(&self, y: f64) -> f64 {
        let half_size = self.canvas_size.1 as f64 / 2.0;
        (y - self.center.1) * half_size / -self.scale.1 + half_size
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
        (x - self.canvas_size.0 as f64 / 2.0) / self.scale.0 + self.center.0
    }

    /// Converts a y-value in **canvas** space to a y-value in **graph** space.
    fn y_to_graph(&self, y: f64) -> f64 {
        -(y - self.canvas_size.1 as f64 / 2.0) / self.scale.1 + self.center.1
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
#[derive(Clone, Debug)]
pub struct Graph {
    /// The expressions to draw.
    pub expressions: Vec<Expr>,

    /// The rendering options for the graph.
    pub options: GraphOptions,
}

impl Graph {
    /// Create a new, empty graph.
    pub fn new() -> Graph {
        Graph {
            expressions: Vec::new(),
            options: GraphOptions::default(),
        }
    }

    /// Add an expression to the graph.
    pub fn add(&mut self, expr: Expr) {
        self.expressions.push(expr);
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

        // evaluate expressions and draw as we go
        context.set_source_rgb(1.0, 0.0, 0.0);
        context.set_line_width(5.0);
        let x_bounds = (
            self.options.x_to_graph(0.0),
            self.options.x_to_graph(self.options.canvas_size.0 as f64),
        );
        let step_len = (x_bounds.1 - x_bounds.0) / 1000.0;

        let mut ctxt = Ctxt::default();
        for expr in self.expressions.iter() {
            let mut x = x_bounds.0;
            let mut first_eval = true;

            while x <= x_bounds.1 {
                ctxt.add_var("x", x.into());
                if let Ok(Value::Number(float)) = expr.eval(&mut ctxt).map(|v| v.coerce_real()) {
                    let canvas = self.options.to_canvas(GraphPoint(x, float.to_f64()));
                    if first_eval {
                        context.move_to(canvas.0, canvas.1);
                        first_eval = false;
                    } else {
                        context.line_to(canvas.0, canvas.1);
                    }
                }
                x += step_len;
            }
        }
        context.stroke()?;

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
        let top = context.show_text_align(
            &format!("{}", self.options.center.1 + self.options.scale.1),
            (x, padding),
            (0.0, 1.0),
        )?;

        let bottom = context.show_text_align(
            &format!("{}", self.options.center.1 - self.options.scale.1),
            (x, self.options.canvas_size.1 as f64 - padding),
            (0.0, 0.0),
        )?;

        // left edge, right edge
        let y = origin_canvas.1 + padding;
        let left = context.show_text_align(
            &format!("{}", self.options.center.0 - self.options.scale.0),
            (padding, y),
            (0.0, 1.0),
        )?;

        let right = context.show_text_align(
            &format!("{}", self.options.center.0 + self.options.scale.0),
            (self.options.canvas_size.0 as f64 - padding, y),
            (1.0, 1.0),
        )?;

        Ok(EdgeExtents { top, bottom, left, right })
    }
}
