use crate::doc::{Height, Width};
use crate::log;
use crate::space::Space;
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
// - Measures in the `full` list have `is_full`, and those in `nonfull` do not.
// - In both the `full` and `nonfull` lists, measures are sorted by increasing `last` and
//   increasing `(overflow, height)`.
// - If in one of the `full` or `nonfull` lists, one measure has a smaller `last` and larger
//   `(overflow, height)` than another, then it must be discarded.
// - If a `full` measure has a smaller `last` and larger `(overflow, height)` than a `nonfull`
//   measure, then the full measure must be discarded.
#[derive(Debug, Clone)]
pub struct MeasureSet {
    full: Vec<Measure>,
    nonfull: Vec<Measure>,
}

impl Measure {
    pub fn single_line(len: Width, space: Space) -> Option<Measure> {
        if space.is_full && len > 0 {
            return None;
        }

        let overflow = if space.first < 0 {
            len as Overflow
        } else {
            (len - space.first).max(0) as Overflow
        };
        Some(Measure {
            last: space.first - len,
            height: 0,
            overflow,
            is_full: space.is_full,
        })
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
            is_full: other.is_full || self.is_full && other.height == 0 && other.last == self.last,
        }
    }

    fn badness(self) -> (Overflow, Height) {
        (self.overflow, self.height)
    }
}

impl MeasureSet {
    pub fn new() -> MeasureSet {
        MeasureSet {
            full: vec![],
            nonfull: vec![],
        }
    }

    pub fn one_measure(measure: Measure) -> MeasureSet {
        if measure.is_full {
            MeasureSet {
                full: vec![measure],
                nonfull: vec![],
            }
        } else {
            MeasureSet {
                full: vec![],
                nonfull: vec![measure],
            }
        }
    }

    pub fn best(self) -> Option<Measure> {
        let best = match (self.full.first().copied(), self.nonfull.first().copied()) {
            (None, None) => None,
            (Some(only), None) => Some(only),
            (None, Some(only)) => Some(only),
            (Some(x), Some(y)) => {
                if x.badness() <= y.badness() {
                    Some(x)
                } else {
                    Some(y)
                }
            }
        };
        #[cfg(feature = "logging")]
        match best {
            None => {
                log!("Best of {} = None", self);
            }
            Some(best) => {
                log!("Best of {} = {}", self, best);
            }
        }
        best
    }

    pub fn union(self, other: MeasureSet) -> MeasureSet {
        log!("Merge: {}", self);
        log!("     & {}", other);
        let merge = MergeMeasures::new(self, other);
        let mut full = vec![];
        let mut nonfull = vec![];
        for measure in merge {
            if measure.is_full {
                full.push(measure);
            } else {
                nonfull.push(measure);
            }
        }
        log!(
            "     = {}",
            MeasureSet {
                full: full.clone(),
                nonfull: nonfull.clone(),
            }
        );
        MeasureSet {
            full: full.clone(),
            nonfull: nonfull.clone(),
        }
    }

    /// The function you map must preserve the MeasureSet invariants!
    pub fn map(mut self, f: impl Fn(Measure) -> Measure) -> MeasureSet {
        for measure in &mut self.full {
            *measure = f(*measure);
        }
        for measure in &mut self.nonfull {
            *measure = f(*measure);
        }
        self
    }

    pub fn contains(&self, measure: Measure) -> bool {
        // Could do binary search, not sure if it would be faster
        if measure.is_full {
            self.full.contains(&measure)
        } else {
            self.nonfull.contains(&measure)
        }
    }
}

#[derive(Debug, Clone)]
struct PartialMergeMeasures {
    left: Peekable<std::vec::IntoIter<Measure>>,
    right: Peekable<std::vec::IntoIter<Measure>>,
}

#[derive(Debug, Clone)]
struct MergeMeasures {
    full: Peekable<PartialMergeMeasures>,
    nonfull: Peekable<PartialMergeMeasures>,
}

impl MergeMeasures {
    fn new(left: MeasureSet, right: MeasureSet) -> MergeMeasures {
        MergeMeasures {
            full: PartialMergeMeasures {
                left: left.full.into_iter().peekable(),
                right: right.full.into_iter().peekable(),
            }
            .peekable(),
            nonfull: PartialMergeMeasures {
                left: left.nonfull.into_iter().peekable(),
                right: right.nonfull.into_iter().peekable(),
            }
            .peekable(),
        }
    }
}

impl Iterator for PartialMergeMeasures {
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
                        // - If in one of the `full` or `nonfull` lists, one measure has a smaller
                        // `last` and larger `(overflow, height)` than another, then it must be
                        // discarded.
                        (Greater | Equal, Less | Equal) => {
                            self.right.next();
                        }
                        (Less | Equal, Greater | Equal) => {
                            self.left.next();
                        }
                        // - In both the `full` and `nonfull` lists, measures are sorted by
                        // increasing `last` and increasing `(overflow, height)`.
                        (Less, Less) => return self.left.next(),
                        (Greater, Greater) => return self.right.next(),
                    }
                }
            }
        }
    }
}

impl Iterator for MergeMeasures {
    type Item = Measure;

    fn next(&mut self) -> Option<Measure> {
        use std::cmp::Ordering::*;

        loop {
            match (self.nonfull.peek(), self.full.peek()) {
                (None, None) => return None,
                (Some(_), None) => return self.nonfull.next(),
                (None, Some(_)) => return self.full.next(),
                (Some(nonfull), Some(full)) => {
                    match (
                        full.last.cmp(&nonfull.last),
                        full.badness().cmp(&nonfull.badness()),
                    ) {
                        // - If a `full` measure has a smaller `last` and larger `(overflow,
                        // height)` than a `nonfull` measure, then the full measure must be
                        // discarded.
                        (Less | Equal, Greater | Equal) => {
                            self.full.next();
                        }
                        // Otherwise, order by badness
                        (_, Less | Equal) => return self.full.next(),
                        (_, Greater) => return self.nonfull.next(),
                    }
                }
            }
        }
    }
}

impl IntoIterator for MeasureSet {
    type Item = Measure;
    type IntoIter = std::iter::Chain<std::vec::IntoIter<Measure>, std::vec::IntoIter<Measure>>;

    fn into_iter(self) -> Self::IntoIter {
        self.nonfull.into_iter().chain(self.full.into_iter())
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
        let mut measures = self.full.iter().chain(self.nonfull.iter());
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

#[test]
fn measure_unit_tests() {
    assert_eq!(
        Measure::single_line(
            0,
            Space {
                width: 10,
                first: 3,
                indent: None,
                is_full: false,
            }
        ),
        Some(Measure {
            last: 3,
            height: 0,
            overflow: 0,
            is_full: false,
        })
    );
    assert_eq!(
        Measure::single_line(
            0,
            Space {
                width: 10,
                first: 3,
                indent: None,
                is_full: true,
            }
        ),
        Some(Measure {
            last: 3,
            height: 0,
            overflow: 0,
            is_full: true,
        })
    );
    assert_eq!(
        Measure::single_line(
            2,
            Space {
                width: 10,
                first: 3,
                indent: None,
                is_full: false,
            }
        ),
        Some(Measure {
            last: 1,
            height: 0,
            overflow: 0,
            is_full: false,
        })
    );
    assert_eq!(
        Measure::single_line(
            2,
            Space {
                width: 10,
                first: 3,
                indent: None,
                is_full: true,
            }
        ),
        None,
    );
    assert_eq!(
        Measure::single_line(
            5,
            Space {
                width: 10,
                first: 3,
                indent: None,
                is_full: false,
            }
        ),
        Some(Measure {
            last: -2,
            height: 0,
            overflow: 2,
            is_full: false,
        })
    );
    assert_eq!(
        Measure::single_line(
            5,
            Space {
                width: 10,
                first: -3,
                indent: None,
                is_full: false,
            }
        ),
        Some(Measure {
            last: -8,
            height: 0,
            overflow: 5,
            is_full: false,
        })
    );

    assert_eq!(
        Measure::newline(2, 10),
        Measure {
            last: 8,
            height: 1,
            overflow: 0,
            is_full: false,
        }
    );
    assert_eq!(
        Measure::newline(12, 10),
        Measure {
            last: -2,
            height: 1,
            overflow: 2,
            is_full: false,
        }
    );
}
