/// A type that collects the steps of an algorithm.
///
/// [`StepCollector`] is also implemented for the unit type `()`. This is useful when you don't
/// want to know the steps taken by an algorithm, which can enable the solver to use more
/// efficient, less-human-friendly algorithms.
pub trait StepCollector<S> {
    /// Adds a step to the collector.
    fn push(&mut self, step: S);
}

impl<S> StepCollector<S> for () {
    #[inline]
    fn push(&mut self, _: S) {}
}

impl<S> StepCollector<S> for Vec<S> {
    #[inline]
    fn push(&mut self, step: S) {
        self.push(step);
    }
}
