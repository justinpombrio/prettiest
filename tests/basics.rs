use prettiest::{align, flat, nl, pretty, spaces, text, Node, Width};

#[test]
fn test_text() {
    assert_pretty_empty(&text("Hello, world"), 10);
    assert_pretty(&text("Hello, world"), 12, vec!["Hello, world"]);
    assert_pretty(&text("Hello, world"), 20, vec!["Hello, world"]);
}

#[test]
fn test_spaces() {
    assert_pretty_empty(&spaces(3), 2);
    assert_pretty(&spaces(3), 3, vec!["   "]);
}

#[test]
fn test_newline() {
    /*
    let doc = nl();
    assert_pretty(&doc, 0, vec!["", ""]);

    let doc = text("hi") + nl();
    assert_pretty(&doc, 2, vec!["hi", ""]);

    let doc = nl() + text("hi");
    assert_pretty(&doc, 2, vec!["", "hi"]);

    let doc = text("hi") ^ text("there");
    assert_pretty(&doc, 5, vec!["hi", "there"]);
    */

    let doc = (text("hi") ^ text("the")) + (text("re") ^ text("Jill"));
    assert_pretty_empty(&doc, 4);
    assert_pretty(&doc, 80, vec!["hi", "there", "Jill"]);
}

#[test]
fn test_concat() {
    let doc = text("Hello, ") + text("world");
    assert_pretty_empty(&doc, 11);
    assert_pretty(&doc, 12, vec!["Hello, world"]);
    assert_pretty(&doc, 14, vec!["Hello, world"]);
}

////////////////////////////////////////

#[track_caller]
fn assert_pretty_empty(doc: &Node, width: Width) {
    assert_pretty_inner(doc, width, None);
}

#[track_caller]
fn assert_pretty(doc: &Node, width: Width, expected_lines: Vec<&str>) {
    assert_pretty_inner(doc, width, Some(expected_lines));
}

#[track_caller]
fn assert_pretty_inner(doc: &Node, width: Width, expected_lines: Option<Vec<&str>>) {
    let msg = &format!("IN PRETTY PRINTING WITH WIDTH {}", width);
    let expected = match expected_lines {
        Some(expected_lines) => expected_lines.join("\n"),
        None => "(none)".to_string(),
    };
    let actual = match pretty(doc, width) {
        Some(actual_lines) => actual_lines.join("\n"),
        None => "(none)".to_string(),
    };
    compare_lines(msg, expected, actual, None);
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
