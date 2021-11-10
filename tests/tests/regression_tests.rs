use super::common::{assert_invalid, assert_pretty, assert_ugly};
use prettiest::constructors::{align, empty, eol, nl, text};

#[test]
fn regression_1() {
    let doc = text("a") + empty();
    assert_ugly(&doc, 0, "a", 1);
}

#[test]
fn regression_2() {
    let doc = eol() | text("a");
    assert_pretty(&doc, 1, "");
}

#[test]
fn regression_3() {
    let doc = eol() + empty() + text("a");
    assert_invalid(&doc, 0);
}

#[test]
fn regression_4() {
    let doc = eol() + (empty() + text("a"));
    assert_invalid(&doc, 0);
}

#[test]
fn regression_5() {
    let doc = eol() + empty() + text("a");
    assert_invalid(&doc, 1);
}

#[test]
fn regression_6() {
    let doc = (eol() | text("a")) + nl();
    assert_pretty(&doc, 1, "a\n");
}

#[test]
fn regression_7() {
    let doc = (eol() | text("a")) + eol();
    assert_pretty(&doc, 1, "");
}

#[test]
fn regression_8() {
    let doc = eol() + align(nl());
    assert_pretty(&doc, 1, "\n");
}

#[test]
fn regression_9() {
    let doc = (text("a") | eol()) + align(nl() + text("a"));
    assert_pretty(&doc, 1, "\na");
}

#[test]
fn regression_10() {
    let doc = empty() | (nl() + eol());
    assert_pretty(&doc, 0, "");
}
