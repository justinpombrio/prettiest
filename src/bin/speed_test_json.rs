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

fn main() {
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
