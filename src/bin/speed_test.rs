use prettiest::constructors::{nl, text};
use prettiest::pretty_json::{array, null, number, object};
use prettiest::{pretty_print, Doc, PrettyResult};

fn make_json(size: usize) -> Doc<()> {
    if size == 0 {
        return null();
    }

    let numbers = array((0..size).map(|n| number(n as f32)).collect());
    let mut keys = vec![];
    for i in 0..size {
        keys.push((i, format!("child_{}", i)));
    }
    let mut dict = vec![("numbers", numbers)];
    for (i, key) in &keys {
        dict.push((key, make_json(*i)));
    }
    object(dict)
}

fn word_flow<'a>(words: impl Iterator<Item = &'a str>) -> Doc<()> {
    let mut iter = words.into_iter();
    let first_word = iter.next().unwrap();

    let mut flow = text(first_word);
    for word in iter {
        flow = flow + (text(" ") | nl()) + text(word);
    }
    flow
}

fn run_word_flow() {
    println!("Start Big Flow speed test");

    println!("constructing paragraph...");
    let sentence = "The quick brown fox jumps over the lazy dog. ";
    let mut paragraph = "".to_owned();
    for _ in 0..500 {
        paragraph.push_str(sentence);
    }
    paragraph.pop(); // remove final space

    println!("constructing doc...");
    let doc = word_flow(paragraph.split(' '));

    println!("formatting");
    let result = pretty_print(&doc, 88);

    match result {
        PrettyResult::Invalid => panic!("invalid"),
        PrettyResult::Valid { lines, .. } => {
            for line in lines {
                println!("{}", line);
            }
        }
    }
}

fn run_big_json() {
    println!("Start Big Json speed test");

    println!("constructing json...");
    let doc = make_json(5);
    println!("Doc size: {}", doc.size());

    println!("formatting");
    let result = pretty_print(&doc, 80);

    match result {
        PrettyResult::Invalid => panic!("invalid"),
        PrettyResult::Valid { lines, .. } => {
            for line in lines {
                println!("{}", line);
            }
        }
    }
}

fn main() {
    run_big_json()
}
