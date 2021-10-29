use crate::doc::{align, flat, nl, spaces, text, text_owned, Doc};

pub fn null() -> Doc<()> {
    text("null")
}

pub fn bool(b: bool) -> Doc<()> {
    if b {
        text("true")
    } else {
        text("false")
    }
}

pub fn string(s: &str) -> Doc<()> {
    let mut string = "\"".to_owned();
    for ch in s.escape_default() {
        string.push(ch);
    }
    string.push('\"');
    text_owned(string)
}

pub fn number(n: f32) -> Doc<()> {
    text_owned(format!("{}", n))
}

// TODO: force multi-line elems to not share their first or last line?
/// A JSON array. It will flow-wrap its elements, putting as many on each line as fit.
///
/// ```text
///     [elem1, elem2, elem3, elem4,
///      elem5_which_is_longer, elem6,
///      elem7]
/// ```
pub fn array(elems: Vec<Doc<()>>) -> Doc<()> {
    let mut elems = elems.into_iter();
    if let Some(first_elem) = elems.next() {
        let mut list = first_elem;
        for elem in elems {
            list = list + text(",") + (text(" ") | nl()) + elem;
        }
        text("[") + align(list) + text("]")
    } else {
        text("[]")
    }
}

/// A JSON object. It may be printed in a few styles.
///
/// On one line, if it fits:
///
/// ```text
/// { "x": 1.5, "y": -2.5 }
/// ```
///
/// With its keys and values aligned to the right of the brace:
///
/// ```text
/// ...context... { "x": 1.0000000000001,
///                 "y": 1.0000000000002 }
/// ```
///
/// Or with its keys de-dented to give more space:
///
/// ```text
/// ...context... {
///     "x": 1.00000000001,
///     "y": 1.00000000002
/// }
/// ```
///
/// In any case, if a key is too long, its value may start on the next line:
///
/// ```text
/// { "worlds-longest-key-yeah-its-pretty-long":
///       "value's shorter though" }
/// ```
pub fn object<'a>(pairs: Vec<(&'a str, Doc<()>)>) -> Doc<()> {
    fn print_pair(key: &str, val: Doc<()>) -> Doc<()> {
        string(key) + text(":") + (text(" ") | nl() + text("    ")) + align(val)
    }

    if pairs.len() == 0 {
        return text("{}");
    }

    let single_line = {
        let mut pairs = pairs.clone().into_iter();
        let (key1, val1) = pairs.next().unwrap();
        let mut list = print_pair(key1.clone(), val1.clone());
        for (key, val) in pairs {
            list = list + text(", ") + print_pair(key, val);
        }
        flat(text("{ ") + list + text(" }"))
    };

    let aligned = {
        let mut pairs = pairs.clone().into_iter();
        let (key1, val1) = pairs.next().unwrap();
        let mut list = print_pair(key1, val1);
        for (key, val) in pairs {
            list = list + text(",") + nl() + print_pair(key, val);
        }
        text("{ ") + align(list) + text(" }")
    };

    let multi_line = {
        let mut pairs = pairs.clone().into_iter();
        let (key1, val1) = pairs.next().unwrap();
        let mut list = print_pair(key1, val1);
        for (key, val) in pairs {
            list = list + text(",") + nl() + print_pair(key, val);
        }
        text("{") + nl() + spaces(4) + align(list) + nl() + text("}")
    };

    single_line | aligned | multi_line
}
