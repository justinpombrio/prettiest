use super::common::{
    assert_pretty, assert_pretty_multiline, assert_ugly, assert_ugly_multiline, assert_unknown,
};
use prettiest::pretty_json::{array, bool, null, number, object, string};

#[test]
fn test_json_basics() {
    // Primitives
    assert_pretty(&null(), 10, "null");
    assert_pretty(&bool(true), 4, "true");
    assert_ugly(&bool(false), 4, "false", 1);
    assert_pretty(&number(-3.5), 10, "-3.5");
    assert_pretty(&string("hello world"), 80, "\"hello world\"");

    // Arrays
    let doc = array(vec![string("hello"), string("world")]);

    let expected = r#"["hello", "world"]"#;
    assert_pretty(&doc, 18, expected);

    let expected = r#"
["hello",
 "world"]"#;
    assert_pretty_multiline(&doc, 17, expected);

    // Objects
    let doc = object(vec![
        ("message", string("hello")),
        ("recipient", string("world")),
    ]);

    let expected = r#"{ "message": "hello", "recipient": "world" }"#;
    assert_pretty(&doc, 80, expected);

    let expected = r#"
{ "message": "hello",
  "recipient": "world" }"#;
    assert_pretty_multiline(&doc, 40, expected);
}

#[test]
fn test_json_object() {
    let doc = object(vec![("this_is_a_very_long_key", number(17.0))]);
    let expected = r#"
{
    "this_is_a_very_long_key":
        17
}"#;
    assert_pretty_multiline(&doc, 30, expected);

    let doc = object(vec![(
        "kinda_long",
        object(vec![("kinda_long", string("kinda_long"))]),
    )]);
    let expected = r#"
{
    "kinda_long": {
        "kinda_long":
            "kinda_long"
    }
}"#;
    assert_pretty_multiline(&doc, 30, expected);
}

#[test]
fn test_json_wrap() {
    let digits = array(vec![
        number(0.0),
        number(1.0),
        number(2.0),
        number(3.0),
        number(4.0),
        number(5.0),
        number(6.0),
        number(7.0),
        number(8.0),
        number(9.0),
    ]);
    let doc = object(vec![("digits", digits)]);

    let expected = r#"{ "digits": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }"#;
    assert_pretty(&doc, 80, expected);

    let expected = r#"
{ "digits": [0, 1, 2, 3, 4,
             5, 6, 7, 8, 9] }"#;
    assert_pretty_multiline(&doc, 29, expected);

    let expected = r#"
{ "digits": [0, 1, 2,
             3, 4, 5,
             6, 7, 8,
             9] }"#;
    assert_pretty_multiline(&doc, 23, expected);

    let expected = r#"
{ "digits": [0, 1,
             2, 3,
             4, 5,
             6, 7,
             8, 9] }"#;
    assert_pretty_multiline(&doc, 20, expected);
}
