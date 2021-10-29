use crate::doc::Width;
use crate::measure::Measure;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Space {
    /// Total allowed width
    pub width: Width,
    /// Remaining space on the first line
    pub first: Width,
    /// If `None`, we're inside a Flat and newlines are not permitted.
    /// If `Some(ind)`, the indentation level is `ind`, so `ind` spaces must be placed after each
    /// newline.
    pub indent: Option<Width>,
    /// If true, the line is not allowed to get any longer, due to an EndOfLine.
    pub is_full: bool,
}

impl Space {
    pub fn new_rectangle(width: Width) -> Space {
        Space {
            width,
            first: width,
            indent: Some(0),
            is_full: false,
        }
    }

    pub fn indent(mut self, i: Width) -> Space {
        self.indent = self.indent.map(|ind| ind + i);
        self
    }

    pub fn align(self) -> Space {
        self.indent(self.width - self.first)
    }

    pub fn flatten(mut self) -> Space {
        self.indent = None;
        self
    }

    pub fn consume(self, measure: Measure) -> Space {
        if self.indent.is_none() {
            assert_eq!(measure.height, 0, "too tall to fit");
        }
        Space {
            width: self.width,
            first: measure.last,
            indent: self.indent,
            is_full: measure.is_full,
        }
    }
}

impl fmt::Display for Space {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // first:width:indent
        match (self.is_full, self.indent) {
            (false, None) => write!(f, "{}:{}:F", self.first, self.width),
            (true, None) => write!(f, "{}:{}:F.", self.first, self.width),
            (false, Some(indent)) => write!(f, "{}:{}:{}", self.first, self.width, indent),
            (true, Some(indent)) => write!(f, "{}:{}:{}.", self.first, self.width, indent),
        }
    }
}
