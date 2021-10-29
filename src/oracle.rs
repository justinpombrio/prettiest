//! Exponentially slow reference implementation of pretty printing

use crate::doc::{Annotation, Doc, Notation, Width};

#[derive(Debug, Clone)]
struct Layout {
    lines: Vec<String>,
    is_full: bool,
}

pub fn oracular_pretty_print<A: Annotation>(doc: Doc<A>, width: Width) -> Option<Vec<String>> {
    let all_layouts = Layout::empty().layouts(&doc);
    let best_layout = best(width, all_layouts);
    best_layout.map(|layout| layout.lines)
}

impl Layout {
    fn empty() -> Layout {
        Layout {
            lines: vec!["".to_string()],
            is_full: false,
        }
    }

    fn badness(&self, width: Width) -> (usize, usize) {
        let height = self.lines.len();
        let mut overflow = 0;
        for line in &self.lines {
            overflow += line.chars().count().saturating_sub(width as usize);
        }
        (overflow, height)
    }

    fn indent(mut self, ind: Width) -> Layout {
        let mut lines = self.lines.iter_mut();
        lines.next();
        for line in lines {
            *line = format!("{:indent$}{}", "", line, indent = ind as usize);
        }
        self
    }

    fn layouts<A: Annotation>(mut self, doc: &Doc<A>) -> Vec<Layout> {
        use Notation::*;

        match doc.notation.as_ref() {
            Empty => vec![self],
            Text(text) if self.is_full && text.chars().count() > 0 => vec![],
            Text(text) => {
                self.lines.last_mut().unwrap().push_str(text);
                vec![self]
            }
            Spaces(n) => {
                let last_line = self.lines.last_mut().unwrap();
                for _ in 0..*n {
                    last_line.push(' ');
                }
                vec![self]
            }
            Newline => {
                self.lines.push("".to_string());
                self.is_full = false;
                vec![self]
            }
            EndOfLine => {
                self.is_full = true;
                vec![self]
            }
            Indent(i, doc) => self
                .layouts(doc)
                .into_iter()
                .map(|lay| lay.indent(*i))
                .collect(),
            Flat(doc) => self
                .layouts(doc)
                .into_iter()
                .filter(|lay| lay.lines.len() == 1)
                .collect(),
            Align(doc) => {
                let ind = self.lines.last().unwrap().chars().count() as Width;
                self.layouts(doc)
                    .into_iter()
                    .map(|lay| lay.indent(ind))
                    .collect()
            }
            Concat(doc1, doc2) => self
                .layouts(doc1)
                .into_iter()
                .flat_map(|lay1| lay1.layouts(doc2))
                .collect(),
            Choice(doc1, doc2) => {
                let mut docs = self.clone().layouts(doc1);
                docs.append(&mut self.layouts(doc2));
                docs
            }
            Annotate(_, doc) => self.layouts(doc),
        }
    }
}

fn best(width: Width, layouts: Vec<Layout>) -> Option<Layout> {
    if layouts.is_empty() {
        return None;
    }
    let mut layouts = layouts.into_iter();
    let mut best_so_far = {
        let first_layout = layouts.next().unwrap();
        let badness = first_layout.badness(width);
        (badness, first_layout)
    };
    while let Some(layout) = layouts.next() {
        let badness = layout.badness(width);
        if badness < best_so_far.0 {
            best_so_far = (badness, layout);
        }
    }
    Some(best_so_far.1)
}

/*
#[test]
fn super_basic_oracle_test() {
    use crate::doc::{align, nl, text};

    let doc: Doc<()> =
        text("a") + align(text("b") + nl() + text("b")) | text("c") + nl() + text("c");

    assert_eq!(
        oracular_pretty_print(doc.clone(), 80).unwrap(),
        vec!["ab", " b"]
    );
    assert_eq!(
        oracular_pretty_print(doc.clone(), 1).unwrap(),
        vec!["c", "c"]
    );
}
*/
