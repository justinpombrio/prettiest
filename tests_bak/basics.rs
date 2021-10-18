mod common;

use common::{assert_pretty, assert_pretty_multiline, assert_ugly, assert_ugly_multiline};
use prettiest::{align, flat, nl, spaces, text, Badness, Node};

#[test]
fn test_text() {
    let doc = text("Hello, world");
    assert_ugly(&doc, 10, "Hello, world", Badness::overflow(2));
    assert_pretty(&doc, 12, "Hello, world");
    assert_pretty(&doc, 20, "Hello, world");
}

#[test]
fn test_spaces() {
    let doc = spaces(3);
    assert_ugly(&doc, 2, "   ", Badness::overflow(1));
    assert_pretty(&doc, 3, "   ");
}

#[test]
fn test_concat() {
    let doc = text("Hello, ") + text("world");
    assert_ugly(&doc, 11, "Hello, world", Badness::overflow(1));
    assert_pretty(&doc, 12, "Hello, world");
    assert_pretty(&doc, 14, "Hello, world");

    let doc = text("Hello,") + spaces(3) + text("world");
    assert_ugly(&doc, 13, "Hello,   world", Badness::overflow(1));
    assert_pretty(&doc, 14, "Hello,   world");
    assert_pretty(&doc, 15, "Hello,   world");
}

#[test]
fn test_newline() {
    let doc = nl();
    assert_pretty(&doc, 0, "\n");

    let doc = text("hi") + nl();
    assert_pretty(&doc, 2, "hi\n");

    let doc = nl() + text("hi");
    assert_pretty(&doc, 2, "\nhi");

    let doc = text("hi") ^ text("there");
    assert_pretty(&doc, 5, "hi\nthere");

    let doc = (text("hi") ^ text("the")) + (text("re") ^ text("Jill"));
    let expected = "
hi
there
Jill";
    assert_ugly_multiline(&doc, 4, expected, Badness::overflow(1));
    assert_pretty_multiline(&doc, 80, expected);
}

#[test]
fn test_flat() {
    let doc = flat(nl());
    assert_ugly(&doc, 10, "\n", Badness::violations(1));

    let doc = flat(nl()) | text("overflow");
    assert_ugly(&doc, 5, "overflow", Badness::overflow(3));

    let doc = flat(nl()) | text("good") | text("overflow");
    assert_pretty(&doc, 5, "good");

    let doc = flat(nl()) | text("overflow") | text("good");
    assert_pretty(&doc, 5, "good");

    let doc = text("good") | text("overflow") | flat(nl());
    assert_pretty(&doc, 5, "good");
}

#[test]
fn test_align() {
    let doc = text("a") + align(text("b") ^ text("b"));
    let expected = "
ab
 b";
    assert_pretty_multiline(&doc, 2, expected);

    let doc = text("a") + align(text("b") ^ text("b")) + align(text("c") ^ text("c"));
    let expected = "
ab
 bc
  c";
    assert_pretty_multiline(&doc, 3, expected);

    let doc = text("a") + align(text("b") ^ text("b") ^ text("b")) + (text("c") ^ text("c"));
    let expected = "
ab
 b
 bc
c";
    assert_pretty_multiline(&doc, 3, expected);
}

pub fn word_flow<'a>(words: impl Iterator<Item = &'a str>) -> Node {
    let mut iter = words.into_iter();
    let first_word = iter.next().unwrap();

    let mut flow = text(first_word);
    for word in iter {
        flow = flow + (text(" ") | nl()) + text(word);
    }
    flow
}

/*
#[test]
fn test_big_flow() {
    println!("Start big flow test");

    println!("constructing paragraph...");
    let sentence = "The quick brown fox jumps over the lazy dog. ";
    let mut paragraph = "".to_owned();
    for _ in 0..10 {
        paragraph.push_str(sentence);
    }
    paragraph.pop(); // remove final space

    println!("constructing doc...");
    let doc = word_flow(paragraph.split(' '));

    println!("formatting");
    let result = pretty(&doc, 88);

    for line in result.lines {
        println!("{}", line);
    }
}
*/

#[test]
fn test_flow() {
    let doc = word_flow("The quick brown fox jumps over the lazy dog".split(' '));

    let expected = "
The quick brown fox jumps over the lazy dog";
    assert_pretty_multiline(&doc, 80, expected);

    let expected = "
The quick brown fox
jumps over the lazy
dog";
    assert_pretty_multiline(&doc, 20, expected);

    let expected = "
The quick brown
fox jumps over
the lazy dog";
    assert_pretty_multiline(&doc, 15, expected);

    let expected = "
The quick
brown fox
jumps over
the lazy
dog";
    assert_pretty_multiline(&doc, 10, expected);

    let expected = "
The
quick
brown
fox
jumps
over the
lazy dog";
    assert_pretty_multiline(&doc, 8, expected);

    let expected = "
The
quick
brown
fox
jumps
over
the
lazy
dog";
    assert_ugly_multiline(&doc, 4, expected, Badness::overflow(3));
}
