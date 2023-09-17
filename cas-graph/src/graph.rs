use cairo::{Context, Error};
use cas_eval::{ctxt::Ctxt, eval::Eval, value::Value};
use cas_parser::parser::expr::Expr;
use std::collections::HashMap;

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
}

impl Default for GraphOptions {
    fn default() -> GraphOptions {
        GraphOptions {
            canvas_size: CanvasPoint(1000, 1000),
            center: GraphPoint(0.0, 0.0),
            scale: GraphPoint(10.0, 10.0),
        }
    }
}

impl GraphOptions {
    /// Converts a point in **graph** space to **canvas** space.
    pub fn to_canvas(&self, point: GraphPoint<f64>) -> CanvasPoint<f64> {
        let half_size = (
            self.canvas_size.0 as f64 / 2.0,
            self.canvas_size.1 as f64 / 2.0,
        );
        let graph_dist_from_center = (point.0 - self.center.0, point.1 - self.center.1);

        let x = graph_dist_from_center.0 * half_size.0 / self.scale.0 + half_size.0;
        let y = graph_dist_from_center.1 * half_size.1 / -self.scale.1 + half_size.1;
        CanvasPoint(x, y)
    }

    /// Converts an x-value in **canvas** space to an x-value in **graph** space.
    fn x_to_graph(&self, x: f64) -> f64 {
        (x - self.canvas_size.0 as f64 / 2.0) / self.scale.0 + self.center.0
    }

    /// Converts a point in **canvas** space to **graph** space.
    pub fn to_graph(&self, point: CanvasPoint<f64>) -> GraphPoint<f64> {
        let x = self.x_to_graph(point.0);
        let y = -(point.1 - self.canvas_size.1 as f64 / 2.0) / self.scale.1 + self.center.1;
        GraphPoint(x, y)
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
    pub fn draw(&self, context: Context) -> Result<(), Error> {
        context.set_source_rgb(0.0, 0.0, 0.0);
        context.paint()?;

        // draw grid lines
        // vertical grid lines
        context.set_source_rgb(0.4, 0.4, 0.4);
        context.set_line_width(2.0);

        let y_gridlines = 20;
        for x in 0..y_gridlines {
            let x = x as f64;
            let scale = (x / y_gridlines as f64) * self.options.canvas_size.0 as f64;
            context.move_to(scale, 0.0);
            context.line_to(scale, 1000.0);
            context.stroke()?;
        }

        // horizontal grid lines
        let x_gridlines = 20;
        for y in 0..x_gridlines {
            let y = y as f64;
            let scale = (y / x_gridlines as f64) * self.options.canvas_size.1 as f64;
            context.move_to(0.0, scale);
            context.line_to(1000.0, scale);
            context.stroke()?;
        }

        // draw axes x = 0, y = 0 if applicable
        let origin = self.options.to_canvas(GraphPoint(0.0, 0.0));
        context.set_source_rgb(1.0, 1.0, 1.0);
        context.set_line_width(5.0);

        if origin.0 >= 0.0 && origin.0 <= self.options.canvas_size.0 as f64 {
            context.move_to(origin.0, 0.0);
            context.line_to(origin.0, 1000.0);
            context.stroke()?;
        }

        if origin.1 >= 0.0 && origin.1 <= self.options.canvas_size.1 as f64 {
            context.move_to(0.0, origin.1);
            context.line_to(1000.0, origin.1);
            context.stroke()?;
        }

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
}
