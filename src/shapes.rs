use crate::log;
use crate::node::{Height, Width};
use std::fmt;

pub type Count = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Space {
    Flat(Width),
    NotFlat { first: Width, middle: Width },
}

// INVARIANTS:
// - height == 0 -> first == middle == last
// - aligned -> first == middle
// - height == 0 -> aligned
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Shape {
    first: Width,
    middle: Width,
    last: Width,
    aligned: bool,
    height: Height,
    badness: Badness,
}

// Note that due to the Ord derivation, the order of the fields is relevant: they are sorted by
// (violations, overflow) lexicographically.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Badness {
    violations: Count,
    overflow: Count,
}

// INVARIANTS:
// - Shapes are in sorted order: sorted by increasing `last`.
// - If multiple shapes have the same `last`, then only the least bad one is kept.
//
// These invariants may be temporarily ignored while callign `insert`, but must be restored with a
// call to `normalize()` before the ShapeSet is used again.
#[derive(Debug, Clone)]
pub struct ShapeSet(Vec<Shape>);

impl Badness {
    pub fn good() -> Badness {
        Badness {
            violations: 0,
            overflow: 0,
        }
    }

    pub fn overflow(overflow_char_count: Count) -> Badness {
        Badness {
            violations: 0,
            overflow: overflow_char_count,
        }
    }

    pub fn violations(violation_count: Count) -> Badness {
        Badness {
            violations: violation_count,
            overflow: 0,
        }
    }
}

impl Space {
    pub fn new_rectangle(width: Width) -> Space {
        Space::NotFlat {
            first: width,
            middle: width,
        }
    }

    pub fn align(self) -> Space {
        use Space::{Flat, NotFlat};

        match self {
            Flat(len) => Flat(len),
            NotFlat { first, .. } => NotFlat {
                first,
                middle: first,
            },
        }
    }

    pub fn flatten(self) -> Space {
        use Space::{Flat, NotFlat};

        match self {
            Flat(len) => Flat(len),
            NotFlat { first, .. } => Flat(first),
        }
    }

    pub fn consume(self, shape: Shape) -> Space {
        use Space::{Flat, NotFlat};

        match self {
            Flat(available_len) => {
                assert_eq!(shape.height, 0);
                Flat(available_len - shape.last)
            }
            NotFlat { first, middle } => {
                if shape.aligned {
                    NotFlat {
                        first: first - shape.last,
                        middle,
                    }
                } else {
                    NotFlat {
                        first: middle - shape.last,
                        middle,
                    }
                }
            }
        }
    }

    pub fn first(self) -> Width {
        use Space::{Flat, NotFlat};

        match self {
            Flat(len) => len,
            NotFlat { first, .. } => first,
        }
    }
}

impl Shape {
    pub fn single_line(len: Width, available_len: Width) -> Shape {
        Shape {
            first: len,
            middle: len,
            last: len,
            aligned: true,
            height: 0,
            badness: Badness {
                overflow: (len - available_len).max(0) as Count,
                violations: 0,
            },
        }
    }

    pub fn newline() -> Shape {
        Shape {
            first: 0,
            middle: 0,
            last: 0,
            aligned: false,
            height: 1,
            badness: Badness {
                overflow: 0,
                violations: 0,
            },
        }
    }

    pub fn flatten(mut self) -> Shape {
        if self.height > 0 {
            self.badness.violations += 1;
        }
        self
    }

    pub fn align(mut self) -> Shape {
        // TODO: What if last > middle?
        self.middle = self.first;
        self.aligned = true;
        self
    }

    pub fn concat(self, other: Shape) -> Shape {
        // This looks simple but is very subtle: checking it requires looking at 3x3 cases, and
        // verifying that the logic is correct assuming the Shape invariants, and that the
        // invariants are preserved.
        Shape {
            aligned: self.aligned && other.aligned,
            first: if self.aligned {
                self.first.max(self.last + other.first)
            } else {
                self.first
            },
            middle: self.middle.max(self.last + other.first).max(other.middle),
            last: if other.aligned {
                self.last + other.last
            } else {
                other.last
            },
            height: self.height + other.height,
            badness: Badness {
                overflow: self.badness.overflow + other.badness.overflow,
                violations: self.badness.violations + other.badness.violations,
            },
        }
    }

    pub fn badness(self) -> Badness {
        self.badness
    }

    pub fn aligned(self) -> bool {
        self.aligned
    }

    fn is_better(self, other: Shape) -> bool {
        self.last <= other.last && (self.badness, self.height) <= (other.badness, other.height)
    }
}

impl ShapeSet {
    pub fn new() -> ShapeSet {
        ShapeSet(vec![])
    }

    pub fn one_shape(shape: Shape) -> ShapeSet {
        ShapeSet(vec![shape])
    }

    pub fn union(self, mut other: ShapeSet) -> ShapeSet {
        let mut shapes = self.0;
        shapes.append(&mut other.0);
        let mut shape_set = ShapeSet(shapes);
        // TODO: merge efficiently
        shape_set.normalize();
        shape_set
    }

    pub fn normalize(&mut self) {
        //  TODO: do this more efficiently, and in place.
        self.0.sort_by_key(|shape| shape.last);

        // Keep only the best shape for each value of `last`, and eliminate dominated shapes.
        let mut normalized = vec![];
        for i in 0..self.0.len() {
            let shape = self.0[i];
            let mut dominated = false;
            for j in 0..self.0.len() {
                let other_shape = self.0[j];
                if i != j && other_shape.is_better(shape) {
                    dominated = true;
                }
            }
            if !dominated {
                normalized.push(shape);
            }
        }

        log!("normalize: {} -> {}", self, ShapeSet(normalized.clone()));

        self.0 = normalized;
    }

    pub fn contains(&self, shape: Shape) -> bool {
        self.0.contains(&shape)
    }

    /// Must call normalize() after making insertions
    pub fn insert(&mut self, shape: Shape) {
        // TODO: efficiency?
        self.0.push(shape);
    }

    pub fn best(self) -> Shape {
        *self.0.last().unwrap()
    }

    pub fn flatten(mut self) -> ShapeSet {
        for shape in &mut self.0 {
            *shape = shape.flatten();
        }
        self
    }

    pub fn align(mut self) -> ShapeSet {
        for shape in &mut self.0 {
            *shape = shape.align();
        }
        self
    }
}

impl IntoIterator for ShapeSet {
    type Item = Shape;
    type IntoIter = std::vec::IntoIter<Shape>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for Badness {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.violations, self.overflow) {
            (0, 0) => write!(f, ""),
            (v, 0) => write!(f, "!v{}", v),
            (0, o) => write!(f, "!o{}", o),
            (v, o) => write!(f, "!v{}o{}", v, o),
        }
    }
}

impl fmt::Display for Space {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Space::{Flat, NotFlat};

        match self {
            Flat(len) => write!(f, "{}/0", len),
            NotFlat { first, middle } => write!(f, "{}:{}:{}", first, middle, middle),
        }
    }
}

impl fmt::Display for Shape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.aligned {
            write!(f, "a")?;
        }
        write!(
            f,
            "{}:{}:{}/{}{}",
            self.first, self.middle, self.last, self.height, self.badness
        )
    }
}

impl fmt::Display for ShapeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut shapes = self.0.iter();
        if let Some(shape) = shapes.next() {
            write!(f, "{}", shape)?;
            for shape in shapes {
                write!(f, ", {}", shape)?;
            }
        }
        write!(f, "]")
    }
}
