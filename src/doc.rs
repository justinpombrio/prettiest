use rand::random;
use std::fmt;
use std::ops::{Add, BitOr, BitXor, Shr};
use std::rc::Rc;

pub trait Annotation: fmt::Debug + Clone {}

pub type Id = usize;

/// Number of newlines. A single line has height 0.
pub type Height = u32;
pub type Width = i16;

#[derive(Clone)]
pub struct Doc<A: Annotation> {
    pub id: Id,
    pub notation: Rc<Notation<A>>,
}

#[derive(Debug, Clone)]
pub enum Notation<A: Annotation> {
    Empty,
    Text(String),
    Spaces(Width),
    Newline,
    EndOfLine,
    Indent(Width, Doc<A>),
    Flat(Doc<A>),
    Align(Doc<A>),
    Concat(Doc<A>, Doc<A>),
    Choice(Doc<A>, Doc<A>),
    Annotate(A, Doc<A>),
}

impl<A: Annotation> fmt::Display for Notation<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Notation::*;

        match self {
            Empty => write!(f, "ε"),
            Text(t) => write!(f, "\"{}\"", t),
            Spaces(w) => write!(f, "Spaces({})", w),
            Newline => write!(f, "NL"),
            EndOfLine => write!(f, "EOL"),
            Indent(i, d) => write!(f, "{} => {}", i, d),
            Flat(d) => write!(f, "Flat({})", d),
            Align(d) => write!(f, "Align({})", d),
            Concat(d1, d2) => write!(f, "{} + {}", d1, d2),
            Choice(d1, d2) => write!(f, "{} | {}", d1, d2),
            Annotate(_, d) => write!(f, "@{}", d),
        }
    }
}

impl<A: Annotation> fmt::Display for Doc<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.notation)
    }
}

impl<A: Annotation> fmt::Debug for Doc<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.notation)
    }
}

impl<A: Annotation> Doc<A> {
    fn new(notation: Notation<A>) -> Doc<A> {
        Doc {
            id: random(),
            notation: Rc::new(notation),
        }
    }
}

pub fn text_owned<A: Annotation>(s: String) -> Doc<A> {
    Doc::new(Notation::Text(s))
}

pub fn text<A: Annotation>(s: &str) -> Doc<A> {
    text_owned(s.to_owned())
}

pub fn spaces<A: Annotation>(width: Width) -> Doc<A> {
    Doc::new(Notation::Spaces(width))
}

pub fn nl<A: Annotation>() -> Doc<A> {
    Doc::new(Notation::Newline)
}

pub fn flat<A: Annotation>(node: Doc<A>) -> Doc<A> {
    Doc::new(Notation::Flat(node))
}

pub fn align<A: Annotation>(node: Doc<A>) -> Doc<A> {
    Doc::new(Notation::Align(node))
}

impl<A: Annotation> Add<Doc<A>> for Doc<A> {
    type Output = Doc<A>;

    /// Shorthand for `Concat`.
    fn add(self, other: Doc<A>) -> Doc<A> {
        Doc::new(Notation::Concat(self, other))
    }
}

impl<A: Annotation> BitOr<Doc<A>> for Doc<A> {
    type Output = Doc<A>;

    /// Shorthand for `Choice`.
    fn bitor(self, other: Doc<A>) -> Doc<A> {
        Doc::new(Notation::Choice(self, other))
    }
}

impl<A: Annotation> BitXor<Doc<A>> for Doc<A> {
    type Output = Doc<A>;

    /// Shorthand for `X + newline() + Y`.
    fn bitxor(self, other: Doc<A>) -> Doc<A> {
        self + nl() + other
    }
}

impl<A: Annotation> Shr<Doc<A>> for Width {
    type Output = Doc<A>;

    /// Shorthand for nesting
    fn shr(self, node: Doc<A>) -> Doc<A> {
        nl() + spaces(self) + align(node)
    }
}

impl Annotation for () {}
