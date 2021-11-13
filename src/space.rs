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

    pub fn align(mut self) -> Space {
        self.indent = self.indent.map(|_| self.width - self.first);
        self
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
            is_full: measure.is_full
                || self.is_full && measure.height == 0 && measure.last == self.first,
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

#[test]
fn space_unit_tests() {
    // space:   width, first, indent, is_full
    // measure: last, height, overflow, is_full
    let space = Space {
        width: 10,
        first: 3,
        indent: None,
        is_full: false,
    };
    let measure = Measure {
        last: 1,
        height: 0,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 1,
            indent: None,
            is_full: false,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: None,
        is_full: false,
    };
    let measure = Measure {
        last: -2,
        height: 0,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: -2,
            indent: None,
            is_full: false,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: false,
    };
    let measure = Measure {
        last: 4,
        height: 1,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 4,
            indent: Some(2),
            is_full: false,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: true,
    };
    let measure = Measure {
        last: 3,
        height: 0,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 3,
            indent: Some(2),
            is_full: true,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: true,
    };
    let measure = Measure {
        last: 3,
        height: 1,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 3,
            indent: Some(2),
            is_full: false,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: true,
    };
    let measure = Measure {
        last: 3,
        height: 0,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 3,
            indent: Some(2),
            is_full: true,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: true,
    };
    let measure = Measure {
        last: 2,
        height: 0,
        overflow: 0,
        is_full: false,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 2,
            indent: Some(2),
            is_full: false,
        }
    );

    let space = Space {
        width: 10,
        first: 3,
        indent: Some(2),
        is_full: false,
    };
    let measure = Measure {
        last: 2,
        height: 0,
        overflow: 0,
        is_full: true,
    };
    assert_eq!(
        space.consume(measure),
        Space {
            width: 10,
            first: 2,
            indent: Some(2),
            is_full: true,
        }
    );
}
