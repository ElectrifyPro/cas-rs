#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Interval {
    /// Empty interval; no points are contained within.
    Empty,

    /// An interval with a start and end point.
    Range(f64, f64),
}

impl Interval {
    /// Create a new interval from a start and end point.
    ///
    /// The interval is empty if `start > end`.
    pub fn new(start: f64, end: f64) -> Self {
        if start > end {
            Self::Empty
        } else {
            Self::Range(start, end)
        }
    }

    /// Maps an interval into an `Option` if it is not empty.
    pub fn map<F, T>(&self, f: F) -> Option<T>
    where
        F: FnOnce(f64, f64) -> T,
    {
        match self {
            Interval::Empty => None,
            Interval::Range(start, end) => Some(f(*start, *end)),
        }
    }

    /// Creates an empty interval.
    pub fn empty() -> Self {
        Self::Empty
    }

    /// Creates an interval covering the entire positive real number line, including zero.
    pub fn positive() -> Self {
        Self::Range(0.0, f64::INFINITY)
    }

    /// Returns the intersection of two intervals.
    pub fn intersection(self, other: Self) -> Self {
        match (self, other) {
            (Interval::Empty, _) | (_, Interval::Empty) => Interval::Empty,
            (Interval::Range(start1, end1), Interval::Range(start2, end2)) => {
                let start = start1.max(start2);
                let end = end1.min(end2);

                if start > end {
                    Interval::Empty
                } else {
                    Interval::Range(start, end)
                }
            }
        }
    }

    /// Produces an arbitrary measure of the physical area covered by the interval.
    ///
    /// Specifically, this returns the integral of the exponential function over the interval.
    ///
    /// The [`Labeler`] algorithm seeks to place labels for points such that they do not intersect
    /// each other, while also minimizing the area covered by the labels and their connecting
    /// leader lines. It necessarily requires a numerical measure of the area covered by the labels
    /// in order to optimize it.
    ///
    /// The exact area covered by the labels is not important. Rather, we only need a way to compare
    /// different label placements to determine which is better. The article authors use the
    /// exponential function because its integral is easy to compute and it has the desired
    /// properties for this purpose.
    ///
    /// Additionally, the exponential function is strictly decreasing, and approaches zero quickly.
    /// This allows the algorithm to "weight" the area covered such that areas closer to the origin
    /// point are more important than areas further away.
    ///
    /// [`Labeler`]: super::Labeler
    pub fn area(self) -> f64 {
        match self {
            Interval::Empty => 0.0,
            Interval::Range(start, end) => (-start).exp() - (-end).exp(),
        }
    }
}

/// A set of intervals on the real number line unioned together.
///
/// TODO: there are more efficient implementations, search Interval Trees
#[derive(Clone, Debug)]
pub struct IntervalSet {
    intervals: Vec<Interval>,
}

impl IntervalSet {
    /// Create a new interval set with the initial interval.
    pub fn new(interval: Interval) -> Self {
        Self {
            intervals: vec![interval],
        }
    }

    /// Returns the start point of the first interval in the set.
    pub fn start(&self) -> Option<f64> {
        match self.intervals.first() {
            Some(Interval::Range(start, _)) => Some(*start),
            _ => None,
        }
    }

    /// Returns the first interval in the set.
    pub fn first(&self) -> Interval {
        self.intervals.first()
            .copied()
            .unwrap_or(Interval::Empty)
    }

    /// Create an interval set covering the entire positive real number line, including zero.
    pub fn positive() -> Self {
        Self::new(Interval::positive())
    }

    /// Removes the given interval from the set.
    pub fn remove(&mut self, interval: Interval) {
        if interval == Interval::Empty {
            return;
        }

        let mut new_intervals = Vec::new();

        for &current in &self.intervals {
            if let (Interval::Range(curr_start, curr_end), Interval::Range(interval_start, interval_end)) = (current, interval) {
                if curr_start >= interval_end || curr_end <= interval_start {
                    // interval is disjoint from current
                    new_intervals.push(current);
                } else if curr_start < interval_start && curr_end > interval_end {
                    // interval to remove is fully contained within current
                    new_intervals.push(Interval::Range(curr_start, interval_start));
                    new_intervals.push(Interval::Range(interval_end, curr_end));
                } else if curr_start < interval_start {
                    // interval overlaps with right side of current
                    new_intervals.push(Interval::Range(curr_start, interval_start));
                } else if curr_end > interval_end {
                    // interval overlaps with left side of current
                    new_intervals.push(Interval::Range(interval_end, curr_end));
                }
            }
        }

        self.intervals = new_intervals;
    }

    /// Produces an arbitrary measure of the physical area covered by the interval set.
    ///
    /// See [`Interval::area`] for a detailed explanation of the area calculation.
    pub fn area(&self) -> f64 {
        self.intervals.iter()
            .map(|interval| interval.area())
            .sum()
    }

    /// Returns true if the interval set is empty.
    pub fn is_empty(&self) -> bool {
        self.intervals.is_empty()
            || self.intervals.iter().all(|interval| *interval == Interval::Empty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interval_set_remove() {
        let mut set = IntervalSet::new(Interval::Range(0.0, f64::INFINITY));
        set.remove(Interval::Range(1.0, 2.0));
        assert_eq!(set.intervals, vec![Interval::Range(0.0, 1.0), Interval::Range(2.0, f64::INFINITY)]);

        set.remove(Interval::Range(0.0, 1.0));
        assert_eq!(set.intervals, vec![Interval::Range(2.0, f64::INFINITY)]);

        set.remove(Interval::Range(11.5, 12.5));
        assert_eq!(set.intervals, vec![Interval::Range(2.0, 11.5), Interval::Range(12.5, f64::INFINITY)]);

        set.remove(Interval::Range(0.0, 1.0));
        assert_eq!(set.intervals, vec![Interval::Range(2.0, 11.5), Interval::Range(12.5, f64::INFINITY)]);

        set.remove(Interval::Range(5.0, 13.0));
        assert_eq!(set.intervals, vec![Interval::Range(2.0, 5.0), Interval::Range(13.0, f64::INFINITY)]);

        set.remove(Interval::Range(8.0, f64::INFINITY));
        assert_eq!(set.intervals, vec![Interval::Range(2.0, 5.0)]);
    }
}
