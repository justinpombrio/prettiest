mod assertions;
#[macro_use]
mod generators;
mod doc_generator;

pub use assertions::{
    assert_invalid, assert_pretty, assert_pretty_multiline, assert_ugly, assert_ugly_multiline,
    assert_unknown,
};
pub use doc_generator::{doc_factory, Factory};
