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

impl<T> From<(T, T)> for CanvasPoint<T> {
    fn from((x, y): (T, T)) -> CanvasPoint<T> {
        CanvasPoint(x, y)
    }
}
