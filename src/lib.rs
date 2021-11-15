mod doc;
mod measure;
mod printer;
mod space;
#[macro_use]
mod infra;

// TODO: Possible optimizations:
// - Smart constructors
// - Flat as a smart constructor
// - Set width-limit=first-limit=0 on overflowing aligns

pub mod oracle;
pub mod pretty_json;

pub use doc::{Annotation, Doc, Height, Id, Notation, Width};
pub use measure::Overflow;
pub use printer::{pretty_print, PrettyResult};

pub mod constructors {
    pub use super::doc::{align, empty, eol, flat, indent, nl, spaces, text, text_owned};
}

#[macro_export]
macro_rules! span {
    ($name:expr) => {
        #[cfg(feature = "flamegraph")]
        no_nonsense_flamegraphs::span!($name);
        #[cfg(not(feature = "flamegraph"))]
        ();
    };
}
