use crate::doc::{align, empty, flat, nested, nl, space, spaces, text, Doc};

// TODO: character escapes

pub struct Xml {
    pub tag: String,
    pub attributes: Vec<(String, String)>,
    pub content: XmlContent,
}

pub enum XmlContent {
    Empty,
    Text(String),
    Sequence(Vec<Xml>),
}

pub fn format_xml(xml: &Xml) -> Doc {
    use XmlContent::{Empty, Sequence, Text};

    let attributes = format_attributes(&xml.attributes);

    // If there's nothing inside the tag, we'll use a self-closing tag.
    if let Empty = &xml.content {
        return text("<") + text(&xml.tag) + attributes + text("/>");
    }

    // Otherwise, we'll use an open and close tag.
    let open = || text("<") + text(&xml.tag) + attributes.clone() + text(">");
    let close = || text("</") + text(&xml.tag) + text(">");

    match &xml.content {
        Text(t) => {
            // Either:
            //   <tag>This is some text.</tag>
            // or:
            //   <tag>
            //       This is
            //       some text.
            //   </tag>
            let single_line = flat(open() + word_flow(t) + close());
            let multi_line = open() + nested(4, word_flow(t)) + close();
            single_line | multi_line
        }
        Sequence(seq) => {
            // <tag>
            //     <inner-tag/>
            //     <another-tag/>
            // </tag>
            let mut iter = seq.into_iter();
            let mut list = format_xml(iter.next().expect("xml seq must be nonempty"));
            for xml in iter {
                list = list + nl() + format_xml(xml);
            }
            open() + nested(4, list) + close()
        }
        Empty => unreachable!(),
    }
}

fn format_attributes(attributes: &[(String, String)]) -> Doc {
    fn quoted(val: &str) -> Doc {
        text("\"") + text(val) + text("\"")
    }

    fn flat_attribute(key: &str, val: &str) -> Doc {
        text(key) + text("=") + quoted(val)
    }

    fn attribute(key: &str, val: &str) -> Doc {
        text(key) + (empty() | nl() + spaces(2)) + text("=") + quoted(val)
    }

    if attributes.is_empty() {
        return empty();
    }

    let single_line = {
        // <p key1="val1" key2="val2">
        let mut attr_list = empty();
        for (key, val) in attributes {
            attr_list = attr_list + space() + flat_attribute(key, val);
        }
        attr_list
    };

    let aligned = {
        // <paragraph long_key_1="val1"
        //            key2="val2">
        let mut iter = attributes.into_iter().map(|(k, v)| flat_attribute(k, v));
        let mut attr_list = iter.next().unwrap();
        for attr in iter {
            attr_list = attr_list + nl() + attr;
        }
        space() + align(attr_list)
    };

    let multi_line = {
        // <paragraph
        //   long_key_1
        //     ="val1"
        //   key2="val2">
        let mut iter = attributes.into_iter().map(|(k, v)| attribute(k, v));
        let mut attr_list = iter.next().unwrap();
        for attr in iter {
            attr_list = attr_list + nl() + attr;
        }
        nl() + spaces(2) + align(attr_list)
    };

    single_line | aligned | multi_line
}

fn word_flow(string: &str) -> Doc {
    let mut words = string.split_whitespace();
    if let Some(first_word) = words.next() {
        let mut flow = text(first_word);
        for word in words {
            flow = flow + (space() | nl()) + text(word);
        }
        flow
    } else {
        empty()
    }
}
