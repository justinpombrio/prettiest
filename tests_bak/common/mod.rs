use prettiest::{pretty, Badness, Node, Width};

#[track_caller]
pub fn assert_pretty(doc: &Node, width: Width, expected: &str) {
    let result = pretty(doc, width);

    let msg = &format!("IN PRETTY PRINTING WITH WIDTH {}", width);
    let expected = expected.to_owned();
    let actual = result.lines.join("\n");
    compare_lines(msg, expected, actual, None);

    let badness = result.badness;
    assert_eq!(badness, Badness::good());
}

#[track_caller]
pub fn assert_ugly(doc: &Node, width: Width, expected: &str, expected_badness: Badness) {
    let result = pretty(doc, width);

    let msg = &format!("IN PRETTY PRINTING WITH WIDTH {}", width);
    let expected = expected.to_owned();
    let actual = result.lines.join("\n");
    compare_lines(msg, expected, actual, None);

    let actual_badness = result.badness;
    assert_eq!(actual_badness, expected_badness);
}

#[track_caller]
pub fn assert_pretty_multiline(doc: &Node, width: Width, expected: &str) {
    assert_pretty(doc, width, &expected[1..]);
}

#[track_caller]
pub fn assert_ugly_multiline(doc: &Node, width: Width, expected: &str, expected_badness: Badness) {
    assert_ugly(doc, width, &expected[1..], expected_badness);
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
