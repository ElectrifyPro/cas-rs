use super::point::{CanvasPoint, GraphPoint};

/// Options to use when drawing a graph.
#[derive(Clone, Copy, Debug)]
pub struct GraphOptions {
    /// The width and height of the canvas, in pixels.
    ///
    /// The default value is `(1000, 1000)`.
    pub canvas_size: CanvasPoint<u16>,

    /// The `(x, y)` point at which to center the graph.
    ///
    /// For example, to place the origin at the center of the output image, set this to `(0.0,
    /// 0.0)`.
    ///
    /// This field will automatically be computed when calling
    /// [`Graph::center_on_points`](super::Graph::center_on_points).
    ///
    /// The default value is `(0.0, 0.0)`.
    pub center: GraphPoint<f64>,

    /// The `(x, y)` scale of the graph.
    ///
    /// The scale indicates the distance, in graph units, from the center of the canvas to the edge
    /// of the canvas. For example, when the graph is centered at `(0.0, 0.0)` with a scale of
    /// `(10.0, 10.0)`, the visible graph will be from `(x, y): (-10.0, -10.0)` to `(x, y): (10.0,
    /// 10.0)`.
    ///
    /// This field will automatically be computed when calling
    /// [`Graph::center_on_points`](super::Graph::center_on_points).
    ///
    /// The default value is `(10.0, 10.0)`.
    pub scale: GraphPoint<f64>,

    /// When calling [`Graph::center_on_points`], determines whether to scale the x- and y-axes
    /// together (resulting in a square graph) or independently (resulting in a rectangular graph).
    ///
    /// The default value is `false`.
    pub square_scale: bool,

    /// Whether to label the canvas boundaries with their corresponding graph values.
    ///
    /// The default value is `false`.
    pub label_canvas_boundaries: bool,

    /// The number of graph units between each major grid line, given as a pair of `(x, y)` units.
    ///
    /// For example, to have a major grid line every `3.0` units on the x-axis and every `2.0`
    /// units on the y-axis, set this to `(3.0, 2.0)`.
    ///
    /// This field will automatically be computed when calling
    /// [`Graph::center_on_points`](super::Graph::center_on_points).
    ///
    /// The default value is `(2.0, 2.0)`.
    pub major_grid_spacing: GraphPoint<f64>,

    /// The number of spaces to divide each major grid line into, given as a pair of `(x, y)`
    /// units. The number of minor grid lines between each major grid line will then be `x - 1` on
    /// the x-axis, and `y - 1` on the y-axis.
    ///
    /// For example, to divide each major grid line into `5` spaces on the x-axis (4 minor grid
    /// lines) and `4` spaces on the y-axis (3 minor grid lines), set this to `(5, 4)`.
    ///
    /// This field will automatically be computed when calling
    /// [`Graph::center_on_points`](super::Graph::center_on_points).
    ///
    /// The default value is `(4, 4)`.
    pub major_grid_divisions: (u8, u8),

    /// The opacity of the major grid lines, given as a value in the range `0.0` to `1.0`, where
    /// `0.0` is fully transparent and `1.0` is fully opaque. This also affects the opacity of the
    /// major grid line numbers.
    ///
    /// The default value is `0.8`.
    pub major_grid_opacity: f64,

    /// The opacity of the minor grid lines, given as a value in the range `0.0` to `1.0`, where
    /// `0.0` is fully transparent and `1.0` is fully opaque.
    ///
    /// The default value is `0.5`.
    pub minor_grid_opacity: f64,
}

/// The default options for a graph. Returns a [`GraphOptions`] with the following values:
///
/// - [`canvas_size`](GraphOptions::canvas_size): `(1000, 1000)`
/// - [`center`](GraphOptions::center): `(0.0, 0.0)`
/// - [`scale`](GraphOptions::scale): `(10.0, 10.0)`
/// - [`square_scale`](GraphOptions::square_scale): `false`
/// - [`label_canvas_boundaries`](GraphOptions::label_canvas_boundaries): `false`
/// - [`major_grid_spacing`](GraphOptions::major_grid_spacing): `(2.0, 2.0)`
/// - [`major_grid_divisions`](GraphOptions::major_grid_divisions): `(4, 4)`
impl Default for GraphOptions {
    fn default() -> GraphOptions {
        GraphOptions {
            canvas_size: CanvasPoint(1000, 1000),
            center: GraphPoint(0.0, 0.0),
            scale: GraphPoint(10.0, 10.0),
            square_scale: false,
            label_canvas_boundaries: false,
            major_grid_spacing: GraphPoint(2.0, 2.0),
            major_grid_divisions: (4, 4),
            major_grid_opacity: 0.8,
            minor_grid_opacity: 0.4,
        }
    }
}

impl GraphOptions {
    /// Set the canvas size. Returns an updated [`GraphOptions`] for chaining.
    pub fn canvas_size(mut self, width: u16, height: u16) -> Self {
        self.canvas_size = CanvasPoint(width, height);
        self
    }

    /// Set the center of the graph. Returns an updated [`GraphOptions`] for chaining.
    pub fn center(mut self, x: f64, y: f64) -> Self {
        self.center = GraphPoint(x, y);
        self
    }

    /// Set the scale of the graph. Returns an updated [`GraphOptions`] for chaining.
    pub fn scale(mut self, x: f64, y: f64) -> Self {
        self.scale = GraphPoint(x, y);
        self
    }

    /// Set whether to scale the x- and y-axes together. Returns an updated [`GraphOptions`] for
    /// chaining.
    pub fn square_scale(mut self, square_scale: bool) -> Self {
        self.square_scale = square_scale;
        self
    }

    /// Set whether to label the canvas boundaries with their corresponding graph values. Returns
    /// an updated [`GraphOptions`] for chaining.
    pub fn label_canvas_boundaries(mut self, label_canvas_boundaries: bool) -> Self {
        self.label_canvas_boundaries = label_canvas_boundaries;
        self
    }

    /// Set the number of graph units between each major grid line. Returns an updated
    /// [`GraphOptions`] for chaining.
    pub fn major_grid_spacing(mut self, x: f64, y: f64) -> Self {
        self.major_grid_spacing = GraphPoint(x, y);
        self
    }

    /// Set the number of spaces to divide each major grid line into. Returns an updated
    /// [`GraphOptions`] for chaining.
    pub fn major_grid_divisions(mut self, x: u8, y: u8) -> Self {
        self.major_grid_divisions = (x, y);
        self
    }

    /// Set the opacity of the major grid lines. Returns an updated [`GraphOptions`] for chaining.
    pub fn major_grid_opacity(mut self, major_grid_opacity: f64) -> Self {
        self.major_grid_opacity = major_grid_opacity;
        self
    }

    /// Set the opacity of the minor grid lines. Returns an updated [`GraphOptions`] for chaining.
    pub fn minor_grid_opacity(mut self, minor_grid_opacity: f64) -> Self {
        self.minor_grid_opacity = minor_grid_opacity;
        self
    }
}

impl GraphOptions {
    /// Converts an x-value in **graph** space to an x-value in **canvas** space.
    pub(crate) fn x_to_canvas(&self, x: f64) -> f64 {
        let graph_space_range = self.scale.0 * 2.0;

        // normalize x-value to [0.0, 1.0], where 0.0 indicates left-edge of visible graph, 1.0
        // indicates right-edge of visible graph
        let normalized = (x - self.center.0) / graph_space_range + 0.5;

        // convert normalized x-value to canvas space
        normalized * self.canvas_size.0 as f64
    }

    /// Converts a y-value in **graph** space to a y-value in **canvas** space.
    pub(crate) fn y_to_canvas(&self, y: f64) -> f64 {
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
    pub(crate) fn x_to_graph(&self, x: f64) -> f64 {
        // normalize x-value to [0.0, 1.0], where 0.0 indicates left-edge of canvas, 1.0 indicates
        // right-edge of canvas (x should always be positive)
        let normalized = x / self.canvas_size.0 as f64;

        // convert normalized x-value to graph space
        let graph_space_range = self.scale.0 * 2.0;
        let left_edge_graph_space = self.center.0 - self.scale.0;

        normalized * graph_space_range + left_edge_graph_space
    }

    /// Converts a y-value in **canvas** space to a y-value in **graph** space.
    pub(crate) fn y_to_graph(&self, y: f64) -> f64 {
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

#[cfg(test)]
mod tests {
    use crate::graph::round_to;
    use super::*;

    /// Test the conversion functions from canvas to graph space.
    #[test]
    fn canvas_to_graph() {
        let options = GraphOptions {
            canvas_size: CanvasPoint(465, 917),
            center: GraphPoint(-3.0, 2.41),
            scale: GraphPoint(3.59, 5.69),
            ..Default::default()
        };

        assert_eq!(
            options.x_to_graph(0.0),
            options.center.0 - options.scale.0,
        );
        assert_eq!(
            options.x_to_graph(options.canvas_size.0 as f64),
            options.center.0 + options.scale.0,
        );
        assert_eq!(
            options.y_to_graph(0.0),
            options.center.1 + options.scale.1,
        );
        assert_eq!(
            options.y_to_graph(options.canvas_size.1 as f64),
            options.center.1 - options.scale.1,
        );
    }

    /// Test the conversion functions from graph to canvas space.
    #[test]
    fn graph_to_canvas() {
        let options = GraphOptions {
            canvas_size: CanvasPoint(465, 917),
            center: GraphPoint(-3.0, 2.41),
            scale: GraphPoint(3.59, 5.69),
            ..Default::default()
        };

        assert_eq!(
            options.x_to_canvas(options.center.0 - options.scale.0),
            0.0,
        );
        assert_eq!(
            options.x_to_canvas(options.center.0 + options.scale.0),
            options.canvas_size.0 as f64,
        );
        // precision is wonky with this one
        assert_eq!(
            round_to(options.y_to_canvas(options.center.1 + options.scale.1), 1e-6),
            0.0,
        );
        assert_eq!(
            options.y_to_canvas(options.center.1 - options.scale.1),
            options.canvas_size.1 as f64,
        );
    }
}
