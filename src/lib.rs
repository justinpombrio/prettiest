mod doc;
mod measure;
mod printer;
mod space;
#[macro_use]
mod infra;

pub mod oracle;
pub mod pretty_json;

pub use doc::{Annotation, Doc, Height, Id, Notation, Width};
pub use measure::Overflow;
pub use printer::{pretty_print, PrettyResult};

pub mod constructors {
    pub use super::doc::{align, empty, eol, flat, indent, nl, spaces, text, text_owned};
}
