use prettiest::constructors::{nl, text};
use prettiest::{pretty_print, Doc, PrettyResult};

fn word_flow<'a>(words: impl Iterator<Item = &'a str>) -> Doc<()> {
    let mut iter = words.into_iter();
    let first_word = iter.next().unwrap();

    let mut flow = text(first_word);
    for word in iter {
        flow = flow + (text(" ") | nl()) + text(word);
    }
    flow
}

fn main() {
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
