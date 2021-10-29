use crate::doc::{Height, Width};
use crate::log;
use std::fmt;
use std::iter::Peekable;

pub type Overflow = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Measure {
    /// The remaining width on the last line.
    pub last: Width,
    /// The number of newlines.
    pub height: Height,
    /// The number of characters of overflow.
    pub overflow: Overflow,
    /// If true, the line is not allowed to get any longer, due to an EndOfLine.
    pub is_full: bool,
}

// INVARIANTS:
// - Measures are sorted by increasing `(!eol, last)` and increasing `(overflow, height)`.
// - If one measure has a smaller `(!eol, last)` and larger `(overflow, height)` than another in
// the set, then it must be discarded.
#[derive(Debug, Clone)]
pub struct MeasureSet(Vec<Measure>);

impl Measure {
    pub fn single_line(len: Width, available_len: Width) -> Measure {
        Measure {
            last: available_len - len,
            height: 0,
            overflow: (len - available_len).max(0) as Overflow,
            is_full: false,
        }
    }

    pub fn newline(indent: Width, width: Width) -> Measure {
        Measure {
            last: width - indent,
            height: 1,
            overflow: (indent - width).max(0) as Overflow,
            is_full: false,
        }
    }

    pub fn concat(self, other: Measure) -> Measure {
        Measure {
            last: other.last,
            height: self.height + other.height,
            overflow: self.overflow + other.overflow,
            is_full: other.is_full,
        }
    }

    fn key(self) -> (bool, Width) {
        (!self.is_full, self.last)
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

    pub fn best(self) -> Option<Measure> {
        self.0.first().map(|m| *m)
    }

    pub fn filter<F: Fn(&Measure) -> bool>(self, func: F) -> MeasureSet {
        // Every valid (obeying the invariants) subsequence is itself valid.
        MeasureSet(self.0.into_iter().filter(func).collect::<Vec<_>>())
    }

    pub fn union(self, other: MeasureSet) -> MeasureSet {
        log!("Merge: {}", self);
        log!("     & {}", other);
        let merge = MergeMeasures::new(self, other);
        log!("     = {}", MeasureSet(merge.clone().collect::<Vec<_>>()));
        MeasureSet(merge.collect::<Vec<_>>())
    }

    /// The function you map must preserve the MeasureSet invariants!
    pub fn map(mut self, f: impl Fn(Measure) -> Measure) -> MeasureSet {
        for measure in &mut self.0 {
            *measure = f(*measure);
        }
        self
    }

    pub fn contains(&self, measure: Measure) -> bool {
        // Could do binary search, not sure if it would be faster
        self.0.contains(&measure)
    }
}

#[derive(Debug, Clone)]
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
                    match (l.key().cmp(&r.key()), l.badness().cmp(&r.badness())) {
                        // - If one measure has a smaller `(!eol, last)` and larger `(overflow,
                        // height)` than another in the set, then it must be discarded.
                        (Greater | Equal, Less | Equal) => {
                            self.right.next();
                        }
                        (Less | Equal, Greater | Equal) => {
                            self.left.next();
                        }
                        // - Measures are sorted by increasing `(!eol, last)` and increasing
                        // `(overflow, height)`.
                        (Less, Less) => return self.left.next(),
                        (Greater, Greater) => return self.right.next(),
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

impl fmt::Display for Measure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // last/height:overflow
        if self.is_full {
            write!(f, "{}/{}!{}.", self.last, self.height, self.overflow)
        } else {
            write!(f, "{}/{}!{}", self.last, self.height, self.overflow)
        }
    }
}

impl fmt::Display for MeasureSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut measures = self.0.iter();
        if let Some(measure) = measures.next() {
            write!(f, "[")?;
            write!(f, "{}", measure)?;
            while let Some(measure) = measures.next() {
                write!(f, ", {}", measure)?;
            }
            write!(f, "]")
        } else {
            write!(f, "[]")
        }
    }
}
