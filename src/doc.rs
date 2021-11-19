use rand::random;
use std::collections::HashSet;
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
    id: Id,
    notation: Rc<Notation<A>>,
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
            Choice(d1, d2) => write!(f, "({} | {})", d1, d2),
            Annotate(_, d) => write!(f, "@{}", d),
        }
    }
}

impl<A: Annotation> Doc<A> {
    pub fn id(&self) -> Id {
        self.id
    }

    pub fn notation(&self) -> &Notation<A> {
        &self.notation
    }

    pub fn size(&self) -> usize {
        let mut set = HashSet::new();
        self.insert_id(&mut set);
        set.len()
    }

    fn insert_id(&self, set: &mut HashSet<Id>) {
        use Notation::*;

        if set.contains(&self.id) {
            return;
        }

        match &*self.notation {
            Empty | Newline | EndOfLine | Spaces(_) | Text(_) => (),
            Indent(_, doc) | Flat(doc) | Align(doc) | Annotate(_, doc) => doc.insert_id(set),
            Concat(x, y) | Choice(x, y) => {
                x.insert_id(set);
                y.insert_id(set);
            }
        }
        set.insert(self.id);
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

pub fn empty<A: Annotation>() -> Doc<A> {
    Doc::new(Notation::Empty)
}

pub fn text_owned<A: Annotation>(s: String) -> Doc<A> {
    Doc::new(Notation::Text(s))
}

pub fn text<A: Annotation>(s: &str) -> Doc<A> {
    text_owned(s.to_owned())
}

pub fn space<A: Annotation>() -> Doc<A> {
    Doc::new(Notation::Spaces(1))
}

pub fn spaces<A: Annotation>(width: Width) -> Doc<A> {
    Doc::new(Notation::Spaces(width))
}

pub fn nl<A: Annotation>() -> Doc<A> {
    Doc::new(Notation::Newline)
}

pub fn eol<A: Annotation>() -> Doc<A> {
    Doc::new(Notation::EndOfLine)
}

pub fn indent<A: Annotation>(ind: Width, node: Doc<A>) -> Doc<A> {
    Doc::new(Notation::Indent(ind, node))
}

pub fn flat<A: Annotation>(node: Doc<A>) -> Doc<A> {
    Doc::new(Notation::Flat(node))
}

pub fn align<A: Annotation>(node: Doc<A>) -> Doc<A> {
    Doc::new(Notation::Align(node))
}

pub fn nested<A: Annotation>(ind: Width, node: Doc<A>) -> Doc<A> {
    indent(ind, nl() + node) + nl()
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
