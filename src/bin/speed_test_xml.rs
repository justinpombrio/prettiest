use prettiest::pretty_xml::{format_xml, Xml, XmlContent};
use prettiest::{pretty_print, PrettyResult};

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

fn make_xml(counter: &mut usize, size: usize) -> Xml {
    use XmlContent::{Empty, Sequence, Text};

    let id = *counter;
    *counter += 1;

    let plaque = tag(
        "plaque",
        vec![],
        Text("Welcome to the labyrinth".to_owned()),
    );

    let mut subrooms = vec![];
    for i in 0..size {
        subrooms.push(make_xml(counter, i));
    }
    let connections = if subrooms.is_empty() {
        tag("connections", vec![], Empty)
    } else {
        tag("connections", vec![], Sequence(subrooms))
    };

    tag(
        "room",
        vec![
            ("id", &format!("{}", id)),
            ("connections", &format!("{}", size)),
            ("transitiveConnections", &format!("{}", *counter - id)),
        ],
        Sequence(vec![plaque, connections]),
    )
}

fn main() {
    println!("Start Big Xml speed test");

    println!("constructing xml...");
    let mut counter = 0;
    let doc = format_xml(&make_xml(&mut counter, 15));
    println!("Doc size: {}", doc.size());

    println!("formatting");
    let result = pretty_print(&doc, 80);

    match result {
        PrettyResult::Invalid => panic!("invalid"),
        PrettyResult::Valid { lines, overflow } => {
            for line in lines {
                println!("{}", line);
            }
            println!("(Overflow: {})", overflow);
        }
    }
}
