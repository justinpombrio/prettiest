use super::common::{assert_pretty, assert_pretty_multiline, assert_ugly_multiline};
use prettiest::pretty_xml::{format_xml, Xml, XmlContent};

fn tag(tag: &str, attrs: Vec<(&str, &str)>, content: XmlContent) -> Xml {
    Xml {
        tag: tag.to_owned(),
        attributes: attrs
            .into_iter()
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect::<Vec<_>>(),
        content,
    }
}

#[test]
fn test_xml_basics() {
    use XmlContent::{Empty, Sequence, Text};

    let xml = format_xml(&tag("tag", vec![], Empty));
    assert_pretty(&xml, 10, "<tag/>");

    let xml = format_xml(&tag("tag", vec![], Text("some text".to_owned())));
    assert_pretty(&xml, 20, "<tag>some text</tag>");

    let expected = r#"
<tag>
    some text
</tag>"#;
    assert_pretty_multiline(&xml, 15, expected);

    let xml = format_xml(&tag(
        "outer",
        vec![],
        Sequence(vec![
            tag("inner", vec![], Empty),
            tag("more", vec![], Empty),
        ]),
    ));
    let expected = r#"
<outer>
    <inner/>
    <more/>
</outer>"#;
    assert_pretty_multiline(&xml, 80, expected);
}

#[test]
fn test_xml_attributes() {
    use XmlContent::Empty;

    let xml = format_xml(&tag(
        "thisTagHasAttributes",
        vec![("aLongAttribute", "aLongValue"), ("short", "17")],
        Empty,
    ));

    let expected = r#"<thisTagHasAttributes aLongAttribute="aLongValue" short="17"/>"#;
    assert_pretty(&xml, 80, expected);

    let expected = r#"
<thisTagHasAttributes aLongAttribute="aLongValue"
                      short="17"/>"#;
    assert_pretty_multiline(&xml, 60, expected);

    let expected = r#"
<thisTagHasAttributes
  aLongAttribute="aLongValue"
  short="17"/>"#;
    assert_pretty_multiline(&xml, 30, expected);

    let expected = r#"
<thisTagHasAttributes
  aLongAttribute
    ="aLongValue"
  short="17"/>"#;
    assert_pretty_multiline(&xml, 25, expected);
}

#[test]
fn test_xml_wrap() {
    use XmlContent::Text;

    let xml = format_xml(&tag(
        "paragraph",
        vec![],
        Text("The quick brown fox jumped over the lazy dog.".to_owned()),
    ));

    let expected = r#"
<paragraph>The quick brown fox jumped over the lazy dog.</paragraph>"#;
    assert_pretty_multiline(&xml, 80, expected);

    let expected = r#"
<paragraph>
    The quick brown fox jumped over the lazy dog.
</paragraph>"#;
    assert_pretty_multiline(&xml, 60, expected);

    let expected = r#"
<paragraph>
    The quick brown fox jumped over the
    lazy dog.
</paragraph>"#;
    assert_pretty_multiline(&xml, 40, expected);

    let expected = r#"
<paragraph>
    The quick brown fox jumped
    over the lazy dog.
</paragraph>"#;
    assert_pretty_multiline(&xml, 30, expected);

    let expected = r#"
<paragraph>
    The quick brown
    fox jumped over
    the lazy dog.
</paragraph>"#;
    assert_pretty_multiline(&xml, 20, expected);
}

#[test]
fn bigger_xml_test() {
    use XmlContent::{Empty, Sequence};

    let book1 = tag(
        "book",
        vec![
            ("title", "The Little Prince"),
            ("author", "Antoine de Saint-Exupéry"),
        ],
        Empty,
    );
    let book2 = tag(
        "book",
        vec![
            ("title", "Computable Calculus"),
            ("author", "Oliver Aberth"),
        ],
        Empty,
    );
    let library = tag(
        "library",
        vec![("numberOfBooks", "2"), ("checkoutPolicy", "default")],
        Sequence(vec![book1, book2]),
    );
    let wrapper1 = tag("testCase", vec![], Sequence(vec![library]));
    let wrapper2 = tag(
        "testCases",
        vec![
            ("purpose", "Testing Xml Printing"),
            ("numberOfTestCases", "1"),
            ("boilerplate", "true"),
        ],
        Sequence(vec![wrapper1]),
    );
    let xml = format_xml(&wrapper2);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2" checkoutPolicy="default">
            <book title="The Little Prince" author="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus" author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 79, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2" checkoutPolicy="default">
            <book title="The Little Prince"
                  author="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus" author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 78, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2" checkoutPolicy="default">
            <book title="The Little Prince"
                  author="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus"
                  author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 69, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2"
                 checkoutPolicy="default">
            <book title="The Little Prince"
                  author="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus"
                  author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 59, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2"
                 checkoutPolicy="default">
            <book
              title="The Little Prince"
              author="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus"
                  author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 52, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2"
                 checkoutPolicy="default">
            <book
              title="The Little Prince"
              author
                ="Antoine de Saint-Exupéry"/>
            <book title="Computable Calculus"
                  author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_pretty_multiline(&xml, 48, expected);

    let expected = r#"
<testCases purpose="Testing Xml Printing"
           numberOfTestCases="1"
           boilerplate="true">
    <testCase>
        <library numberOfBooks="2"
                 checkoutPolicy="default">
            <book
              title="The Little Prince"
              author
                ="Antoine de Saint-Exupéry"/>
            <book
              title="Computable Calculus"
              author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_ugly_multiline(&xml, 44, expected, 1);

    let expected = r#"
<testCases
  purpose="Testing Xml Printing"
  numberOfTestCases="1"
  boilerplate="true">
    <testCase>
        <library
          numberOfBooks="2"
          checkoutPolicy="default">
            <book
              title="The Little Prince"
              author
                ="Antoine de Saint-Exupéry"/>
            <book
              title
                ="Computable Calculus"
              author="Oliver Aberth"/>
        </library>
    </testCase>
</testCases>"#;
    assert_ugly_multiline(&xml, 40, expected, 5);
}
