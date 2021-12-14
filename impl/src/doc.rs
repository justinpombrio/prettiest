use rand::random;
use std::collections::HashSet;
use std::fmt;
use std::ops::{Add, BitOr, BitXor, Shr};
use std::rc::Rc;

pub type Id = usize;

/// Number of newlines. A single line has height 0.
pub type Height = u32;
pub type Width = i16;

#[derive(Clone)]
pub struct Doc {
    id: Id,
    notation: Rc<Notation>,
}

#[derive(Debug, Clone)]
pub enum Notation {
    Empty,
    Text(String),
    Spaces(Width),
    Newline,
    EndOfLine,
    Indent(Width, Doc),
    Flat(Doc),
    Align(Doc),
    Concat(Doc, Doc),
    Choice(Doc, Doc),
}

impl fmt::Display for Notation {
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
        }
    }
}

impl Doc {
    pub fn id(&self) -> Id {
        self.id
    }

    pub fn notation(&self) -> &Notation {
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
            Indent(_, doc) | Flat(doc) | Align(doc) => doc.insert_id(set),
            Concat(x, y) | Choice(x, y) => {
                x.insert_id(set);
                y.insert_id(set);
            }
        }
        set.insert(self.id);
    }
}

impl fmt::Display for Doc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.notation)
    }
}

impl fmt::Debug for Doc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.notation)
    }
}

impl Doc {
    fn new(notation: Notation) -> Doc {
        Doc {
            id: random(),
            notation: Rc::new(notation),
        }
    }
}

pub fn empty() -> Doc {
    Doc::new(Notation::Empty)
}

pub fn text_owned(s: String) -> Doc {
    Doc::new(Notation::Text(s))
}

pub fn text(s: &str) -> Doc {
    text_owned(s.to_owned())
}

pub fn space() -> Doc {
    Doc::new(Notation::Spaces(1))
}

pub fn spaces(width: Width) -> Doc {
    Doc::new(Notation::Spaces(width))
}

pub fn nl() -> Doc {
    Doc::new(Notation::Newline)
}

pub fn eol() -> Doc {
    Doc::new(Notation::EndOfLine)
}

pub fn indent(ind: Width, node: Doc) -> Doc {
    Doc::new(Notation::Indent(ind, node))
}

pub fn flat(node: Doc) -> Doc {
    Doc::new(Notation::Flat(node))
}

pub fn align(node: Doc) -> Doc {
    Doc::new(Notation::Align(node))
}

pub fn nested(ind: Width, node: Doc) -> Doc {
    indent(ind, nl() + node) + nl()
}

impl Add<Doc> for Doc {
    type Output = Doc;

    /// Shorthand for `Concat`.
    fn add(self, other: Doc) -> Doc {
        Doc::new(Notation::Concat(self, other))
    }
}

impl BitOr<Doc> for Doc {
    type Output = Doc;

    /// Shorthand for `Choice`.
    fn bitor(self, other: Doc) -> Doc {
        Doc::new(Notation::Choice(self, other))
    }
}

impl BitXor<Doc> for Doc {
    type Output = Doc;

    /// Shorthand for `X + newline() + Y`.
    fn bitxor(self, other: Doc) -> Doc {
        self + nl() + other
    }
}

impl Shr<Doc> for Width {
    type Output = Doc;

    /// Shorthand for nesting
    fn shr(self, node: Doc) -> Doc {
        nl() + spaces(self) + align(node)
    }
}
