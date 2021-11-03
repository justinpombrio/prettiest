use super::generators::{gen_inc, gen_map, gen_pair, gen_rec, gen_seq, gen_value};
use crate::gen_choices;
use prettiest::constructors::{align, empty, eol, flat, indent, nl, spaces, text_owned};
use prettiest::{Doc, Width};

pub use super::generators::Factory;

pub fn doc_factory() -> Factory {
    let mut factory = Factory::new();

    let gen_doc = gen_rec::<Doc<()>>();

    // Size 0
    let gen_empty = gen_value::<Doc<()>>(empty());
    let gen_nl = gen_value(nl());
    let gen_eol = gen_value(eol());

    // Size n
    let gen_text = gen_seq(|size| {
        let letter = (((size % 26) as u8) + 96) as char;
        let string = format!("{}", letter).repeat(size as usize);
        text_owned(string)
    });
    let gen_spaces = gen_seq(|size| spaces(size as Width));

    // Size 1 + n * Doc
    let gen_indent = {
        let gen_width = gen_seq(|size| size as Width);
        gen_inc(gen_map(gen_pair(gen_width, gen_doc.clone()), |(w, d)| {
            indent(w, d)
        }))
    };

    // Size 1 + Doc
    let gen_flat = gen_inc(gen_map(gen_doc.clone(), flat));
    let gen_align = gen_inc(gen_map(gen_doc.clone(), align));

    // Size 1 + Doc * Doc
    let gen_concat = gen_inc(gen_map(
        gen_pair(gen_doc.clone(), gen_doc.clone()),
        |(d1, d2)| d1 + d2,
    ));
    let gen_choice = gen_inc(gen_map(
        gen_pair(gen_doc.clone(), gen_doc.clone()),
        |(d1, d2)| d1 | d2,
    ));

    factory.register(gen_choices![
        gen_empty, gen_text, gen_spaces, gen_nl, gen_eol, gen_indent, gen_flat, gen_align,
        gen_concat, gen_choice
    ]);

    factory
}

#[test]
fn test_doc_factory() {
    let mut factory = doc_factory();

    assert_eq!(factory.count::<Doc<()>>(0), 5);
    assert_eq!(factory.iter::<Doc<()>>(0).count(), 5);
    assert_eq!(factory.count::<Doc<()>>(1), 67);
    assert_eq!(factory.iter::<Doc<()>>(1).count(), 67);
}
