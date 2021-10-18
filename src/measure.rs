use crate::doc::{Height, Width};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Space {
    pub first: Width,
    pub middle: Option<Width>,
    pub is_full: bool,
}

pub type Overflow = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Measure {
    pub last: Width,
    pub height: Height,
    pub overflow: Overflow,
    pub is_full: bool,
}

// INVARIANTS:
// - If one measure has both a larger `last` and `(overflow, height)` than another, then in the
// set, then it must be discarded.
// - Measures are sorted by increasing `last`.
// - If multiple shapes have the same `last`, then only the one with the least `(overflow, height)`
// is kept.
#[derive(Debug, Clone)]
pub struct MeasureSet(Vec<Measure>);

impl Space {
    pub fn new_rectangle(width: Width) -> Space {
        Space {
            first: width,
            middle: Some(width),
            is_full: false,
        }
    }

    pub fn indent(mut self, ind: Width) -> Space {
        self.middle = self.middle.map(|len| len + ind);
        self
    }

    pub fn align(mut self) -> Space {
        self.middle = self.middle.map(|_| self.first);
        self
    }

    pub fn flatten(mut self) -> Space {
        self.middle = None;
        self
    }

    pub fn consume(self, measure: Measure) -> Space {
        if self.middle.is_none() {
            assert_eq!(measure.height, 0, "too tall to fit");
        }
        Space {
            first: measure.last,
            middle: self.middle,
            is_full: measure.is_full,
        }
    }
}

impl Measure {
    pub fn single_line(len: Width, available_len: Width) -> Measure {
        Measure {
            last: available_len - len,
            height: 0,
            overflow: len.saturating_sub(available_len) as Overflow,
            is_full: false,
        }
    }

    pub fn newline() -> Measure {
        Measure {
            last: 0,
            height: 1,
            overflow: 0,
            is_full: false,
        }
    }

    fn badness(self) -> (Overflow, Height) {
        (self.overflow, self.height)
    }
}

impl MeasureSet {
    pub fn new() -> MeasureSet {
        MeasureSet(vec![])
    }

    pub fn one_measure(measure: Measure) -> MeasureSet {
        MeasureSet(vec![measure])
    }

    pub fn best(self) -> Measure {
        *self.0.last().unwrap()
    }

    pub fn filter<F: Fn(&Measure) -> bool>(self, func: F) -> MeasureSet {
        MeasureSet(self.0.into_iter().filter(func).collect::<Vec<_>>())
    }

    pub fn union(self, other: MeasureSet) -> MeasureSet {
        let merge = MergeMeasures::new(self, other);
        MeasureSet(merge.collect::<Vec<_>>())
    }
}

struct MergeMeasures {
    left: Peekable<std::vec::IntoIter<Measure>>,
    right: Peekable<std::vec::IntoIter<Measure>>,
}

impl MergeMeasures {
    fn new(left: MeasureSet, right: MeasureSet) -> MergeMeasures {
        MergeMeasures {
            left: left.0.into_iter().peekable(),
            right: right.0.into_iter().peekable(),
        }
    }
}

impl Iterator for MergeMeasures {
    type Item = Measure;

    fn next(&mut self) -> Option<Measure> {
        use std::cmp::Ordering::*;

        loop {
            match (self.left.peek(), self.right.peek()) {
                (None, None) => return None,
                (Some(_), None) => return self.left.next(),
                (None, Some(_)) => return self.right.next(),
                (Some(l), Some(r)) => {
                    match (l.last.cmp(&r.last), l.badness().cmp(&r.badness())) {
                        // If one measure has both a larger `last` and `(overflow, height)` than
                        // another, then in the set, then it must be discarded.
                        (Greater, Greater) => {
                            self.left.next();
                        }
                        (Less, Less) => {
                            self.right.next();
                        }
                        // Measures are sorted by increasing `last`.
                        (Less, _) => return self.left.next(),
                        (Greater, _) => return self.right.next(),
                        // If multiple shapes have the same `last`, then only the one with the least
                        // `(overflow, height)` is kept.
                        (Equal, Greater) => {
                            self.left.next();
                        }
                        (Equal, Less | Equal) => {
                            self.right.next();
                        }
                    }
                }
            }
        }
    }
}

impl IntoIterator for MeasureSet {
    type Item = Measure;
    type IntoIter = std::vec::IntoIter<Measure>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
