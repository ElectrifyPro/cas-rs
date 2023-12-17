/// A pair of `(x, y)` values in **graph** units.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct GraphPoint<T>(pub T, pub T);

impl<T> From<(T, T)> for GraphPoint<T> {
    fn from((x, y): (T, T)) -> GraphPoint<T> {
        GraphPoint(x, y)
    }
}

impl<T> GraphPoint<T>
where
    T: Into<f64> + Copy,
{
    /// Returns the distance between two points.
    pub fn distance(self, other: GraphPoint<T>) -> f64 {
        (self.0.into() - other.0.into()).hypot(self.1.into() - other.1.into())
    }
}

/// A pair of `(x, y)` values in **canvas** units.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct CanvasPoint<T>(pub T, pub T);

impl<T> From<(T, T)> for CanvasPoint<T> {
    fn from((x, y): (T, T)) -> CanvasPoint<T> {
        CanvasPoint(x, y)
    }
}

/// A point to draw in a graph with style information.
///
/// The default point color is red.
#[derive(Clone, Debug, PartialEq)]
pub struct Point<T> {
    /// The coordinates of the point to draw.
    pub coordinates: GraphPoint<T>,

    /// The color of the point, given as an RGB tuple with each value in the
    /// range 0.0 to 1.0.
    pub color: (f64, f64, f64),

    /// The label of the point. If omitted, the point will be labeled with its
    /// coordinates, to a maximum of 3 decimal places for each coordinate.
    pub label: Option<String>,
}

impl Default for Point<f64> {
    fn default() -> Point<f64> {
        Point {
            coordinates: (0.0, 0.0).into(),
            color: (1.0, 0.0, 0.0),
            label: None,
        }
    }
}

impl<T> Point<T>
where
    T: Into<f64> + Copy,
    Point<T>: Default,
{
    /// Creates a new point with the given coordinates.
    pub fn new(coordinates: impl Into<GraphPoint<T>>) -> Point<T> {
        Point {
            coordinates: coordinates.into(),
            ..Default::default()
        }
    }

    /// Sets the color of the point.
    ///
    /// Returns the point itself to allow chaining.
    pub fn with_color(mut self, color: (f64, f64, f64)) -> Point<T> {
        self.color = color;
        self
    }

    /// Sets the label of the point.
    ///
    /// Returns the point itself to allow chaining.
    pub fn with_label(mut self, label: String) -> Point<T> {
        self.label = Some(label);
        self
    }
}

impl<T> From<(T, T)> for Point<T>
where
    Point<T>: Default,
{
    fn from((x, y): (T, T)) -> Point<T> {
        Point {
            coordinates: GraphPoint(x, y),
            ..Default::default()
        }
    }
}
