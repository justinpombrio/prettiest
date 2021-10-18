mod common;

use common::{assert_pretty, assert_pretty_multiline, assert_ugly};
use prettiest::pretty_json::{array, bool, null, number, object, string};

#[test]
fn test_json_literals() {
    assert_pretty(&null(), 10, "null");
    assert_pretty(&bool(true), 10, "true");
    assert_pretty(&bool(false), 10, "false");
    assert_pretty(&number(3.14), 10, "3.14");
    assert_pretty(&string(r#"foo"bar"#), 10, r#""foo\"bar""#);
}

#[test]
fn test_json_array() {
    let expected = r#"
["the", "end",
 "is", "nigh"]"#;
    assert_pretty_multiline(
        &array(vec![
            string("the"),
            string("end"),
            string("is"),
            string("nigh"),
        ]),
        15,
        expected,
    );
}

#[test]
fn test_json_object() {
    let doc = object(vec![
        ("label", string("happy_holidays")),
        (
            "foreground_color",
            object(vec![
                ("red", string("255")),
                ("green", string("0")),
                ("blue", string("0")),
            ]),
        ),
        (
            "background_color",
            object(vec![
                ("color_in_hex_format", string("#00ff00")),
                ("alpha", string("128")),
            ]),
        ),
    ]);
    let expected = r##"
{ "label": "happy_holidays",
  "foreground_color": { "red": "255", "green": "0", "blue": "0" },
  "background_color": { "color_in_hex_format": "#00ff00", "alpha": "128" } }"##;
    //  5   10   15   20   25   30   35   40   45   50   55   60   65   70   75   80
    assert_pretty_multiline(&doc, 80, expected);

    let expected = r##"
{ "label": "happy_holidays",
  "foreground_color": { "red": "255", "green": "0", "blue": "0" },
  "background_color": { "color_in_hex_format": "#00ff00",
                        "alpha": "128" } }"##;
    //  5   10   15   20   25   30   35   40   45   50   55   60   65   70   75   80
    assert_pretty_multiline(&doc, 70, expected);

    let expected = r##"
{ "label": "happy_holidays",
  "foreground_color": { "red": "255",
                        "green": "0",
                        "blue": "0" },
  "background_color": { "color_in_hex_format": "#00ff00",
                        "alpha": "128" } }"##;
    //  5   10   15   20   25   30   35   40   45   50   55   60
    assert_pretty_multiline(&doc, 60, expected);

    let expected = r##"
{ "label": "happy_holidays",
  "foreground_color": { "red": "255",
                        "green": "0",
                        "blue": "0" },
  "background_color": { "color_in_hex_format":
                            "#00ff00",
                        "alpha": "128" } }"##;
    //  5   10   15   20   25   30   35   40   45   50   55   60
    assert_pretty_multiline(&doc, 50, expected);
}
