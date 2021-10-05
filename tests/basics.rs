use prettiest::{align, flat, nl, pretty, spaces, text, Badness, Node, Width};

#[test]
fn test_text() {
    let doc = text("Hello, world");
    assert_ugly(&doc, 10, vec!["Hello, world"], Badness::overflow(2));
    assert_pretty(&doc, 12, vec!["Hello, world"]);
    assert_pretty(&doc, 20, vec!["Hello, world"]);
}

#[test]
fn test_spaces() {
    let doc = spaces(3);
    assert_ugly(&doc, 2, vec!["   "], Badness::overflow(1));
    assert_pretty(&doc, 3, vec!["   "]);
}

#[test]
fn test_concat() {
    let doc = text("Hello, ") + text("world");
    assert_ugly(&doc, 11, vec!["Hello, world"], Badness::overflow(1));
    assert_pretty(&doc, 12, vec!["Hello, world"]);
    assert_pretty(&doc, 14, vec!["Hello, world"]);

    let doc = text("Hello,") + spaces(3) + text("world");
    assert_ugly(&doc, 13, vec!["Hello,   world"], Badness::overflow(1));
    assert_pretty(&doc, 14, vec!["Hello,   world"]);
    assert_pretty(&doc, 15, vec!["Hello,   world"]);
}

#[test]
fn test_newline() {
    let doc = nl();
    assert_pretty(&doc, 0, vec!["", ""]);

    let doc = text("hi") + nl();
    assert_pretty(&doc, 2, vec!["hi", ""]);

    let doc = nl() + text("hi");
    assert_pretty(&doc, 2, vec!["", "hi"]);

    let doc = text("hi") ^ text("there");
    assert_pretty(&doc, 5, vec!["hi", "there"]);

    let doc = (text("hi") ^ text("the")) + (text("re") ^ text("Jill"));
    assert_ugly(&doc, 4, vec!["hi", "there", "Jill"], Badness::overflow(1));
    assert_pretty(&doc, 80, vec!["hi", "there", "Jill"]);
}

#[test]
fn test_flat() {
    let doc = flat(nl());
    assert_ugly(&doc, 10, vec!["", ""], Badness::violations(1));

    let doc = flat(nl()) | text("overflow");
    assert_ugly(&doc, 5, vec!["overflow"], Badness::overflow(3));

    let doc = flat(nl()) | text("good") | text("overflow");
    assert_pretty(&doc, 5, vec!["good"]);

    let doc = flat(nl()) | text("overflow") | text("good");
    assert_pretty(&doc, 5, vec!["good"]);

    let doc = text("good") | text("overflow") | flat(nl());
    assert_pretty(&doc, 5, vec!["good"]);
}

#[test]
fn test_align() {
    let doc = text("a") + align(text("b") ^ text("b"));
    assert_pretty(&doc, 2, vec!["ab", " b"]);

    let doc = text("a") + align(text("b") ^ text("b")) + align(text("c") ^ text("c"));
    assert_pretty(&doc, 3, vec!["ab", " bc", "  c"]);

    let doc = text("a") + align(text("b") ^ text("b") ^ text("b")) + (text("c") ^ text("c"));
    assert_pretty(&doc, 3, vec!["ab", " b", " bc", "c"]);
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

#[test]
fn test_big_flow() {
    println!("Start big flow test");

    println!("constructing paragraph...");
    let sentence = "The quick brown fox jumps over the lazy dog. ";
    let mut paragraph = "".to_owned();
    for _ in 0..1000 {
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

#[test]
fn test_flow() {
    let doc = word_flow("The quick brown fox jumps over the lazy dog".split(' '));
    /*
    assert_pretty(
        &doc,
        80,
        vec!["The quick brown fox jumps over the lazy dog"],
    );
    assert_pretty(
        &doc,
        20,
        vec!["The quick brown fox", "jumps over the lazy", "dog"],
    );
    */
    assert_pretty(
        &doc,
        15,
        vec!["The quick brown", "fox jumps over", "the lazy dog"],
    );
    assert!(false);
    /*
    assert_pretty(
        &doc,
        10,
        vec!["The quick", "brown fox", "jumps over", "the lazy", "dog"],
    );
    assert_pretty(
        &doc,
        8,
        vec![
            "The", "quick", "brown", "fox", "jumps", "over the", "lazy dog",
        ],
    );
    assert_ugly(
        &doc,
        4,
        vec![
            "The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
        ],
        Badness::overflow(4),
    );
    */
}

////////////////////////////////////////

#[track_caller]
fn assert_pretty(doc: &Node, width: Width, expected_lines: Vec<&str>) {
    let result = pretty(doc, width);

    let msg = &format!("IN PRETTY PRINTING WITH WIDTH {}", width);
    let expected = expected_lines.join("\n");
    let actual = result.lines.join("\n");
    compare_lines(msg, expected, actual, None);

    let badness = result.badness;
    assert_eq!(badness, Badness::good());
}

#[track_caller]
fn assert_ugly(doc: &Node, width: Width, expected_lines: Vec<&str>, expected_badness: Badness) {
    let result = pretty(doc, width);

    let msg = &format!("IN PRETTY PRINTING WITH WIDTH {}", width);
    let expected = expected_lines.join("\n");
    let actual = result.lines.join("\n");
    compare_lines(msg, expected, actual, None);

    let actual_badness = result.badness;
    assert_eq!(actual_badness, expected_badness);
}

#[track_caller]
fn compare_lines(message: &str, expected: String, actual: String, doc: Option<&str>) {
    if actual != expected {
        if let Some(doc) = doc {
            eprintln!(
                "{}\nDOC:\n{}\nEXPECTED:\n{}\nACTUAL:\n{}\n=========",
                message, doc, expected, actual,
            );
        } else {
            eprintln!(
                "{}\nEXPECTED:\n{}\nACTUAL:\n{}\n=========",
                message, expected, actual,
            );
        }
        assert_eq!(actual, expected);
    }
}
