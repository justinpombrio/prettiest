use rand::random;
use std::fmt;
use std::ops::{Add, BitOr, BitXor, Shr};

pub type Id = usize;

/// Number of newlines. A single line has height 0.
pub type Height = u32;
pub type Width = u16;

pub struct Node {
    pub id: Id,
    pub notation: Box<Notation>,
}

#[derive(Debug)]
pub enum Notation {
    Text(String),
    Spaces(Width),
    Newline,
    Flat(Node),
    Align(Node),
    Concat(Node, Node),
    Choice(Node, Node),
}

pub const MAX_WIDTH: Width = 256;

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.notation)
    }
}

impl Node {
    fn new(notation: Notation) -> Node {
        Node {
            id: random(),
            notation: Box::new(notation),
        }
    }
}

pub fn text(s: &str) -> Node {
    Node::new(Notation::Text(s.to_owned()))
}

pub fn spaces(width: Width) -> Node {
    Node::new(Notation::Spaces(width))
}

pub fn nl() -> Node {
    Node::new(Notation::Newline)
}

pub fn flat(node: Node) -> Node {
    Node::new(Notation::Flat(node))
}

pub fn align(node: Node) -> Node {
    Node::new(Notation::Align(node))
}

impl Add<Node> for Node {
    type Output = Node;

    /// Shorthand for `Concat`.
    fn add(self, other: Node) -> Node {
        Node::new(Notation::Concat(self, other))
    }
}

impl BitOr<Node> for Node {
    type Output = Node;

    /// Shorthand for `Choice`.
    fn bitor(self, other: Node) -> Node {
        Node::new(Notation::Choice(self, other))
    }
}

impl BitXor<Node> for Node {
    type Output = Node;

    /// Shorthand for `X + newline() + Y`.
    fn bitxor(self, other: Node) -> Node {
        self + nl() + other
    }
}

impl Shr<Node> for Width {
    type Output = Node;

    /// Shorthand for nesting
    fn shr(self, node: Node) -> Node {
        nl() + spaces(self) + align(node)
    }
}
