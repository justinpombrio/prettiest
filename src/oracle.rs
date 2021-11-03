//! Exponentially slow reference implementation of pretty printing

use crate::doc::{Annotation, Doc, Notation, Width};
use crate::measure::Overflow;
use crate::printer::PrettyResult;

#[derive(Debug, Clone)]
struct Layout {
    lines: Vec<String>,
    is_full: bool,
}

pub fn verify_with_oracle<A: Annotation>(
    doc: &Doc<A>,
    width: Width,
    result: &PrettyResult,
) -> Result<(), PrettyResult> {
    let all_layouts = Layout::empty().layouts(&doc, Some(0));

    let valid_layouts = {
        let min_badness = all_layouts.iter().map(|lay| lay.badness(width)).min();
        if let Some(min_badness) = min_badness {
            all_layouts
                .into_iter()
                .filter(|lay| lay.badness(width) == min_badness)
                .collect::<Vec<_>>()
        } else {
            vec![]
        }
    };
    let mut valid_results = valid_layouts
        .into_iter()
        .map(|lay| lay.into_pretty_result(width))
        .collect::<Vec<_>>();

    match &result {
        PrettyResult::Invalid => {
            if let Some(valid_result) = valid_results.drain(..).next() {
                Err(valid_result)
            } else {
                Ok(())
            }
        }
        PrettyResult::Valid { .. } => {
            if valid_results.is_empty() {
                Err(PrettyResult::Invalid)
            } else if valid_results.contains(&result) {
                Ok(())
            } else if let Some(valid_result) =
                valid_results.iter().find(|r| has_same_lines(r, &result))
            {
                Err(valid_result.to_owned())
            } else {
                Err(valid_results.drain(..).next().unwrap())
            }
        }
    }
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

    fn into_pretty_result(self, width: Width) -> PrettyResult {
        PrettyResult::Valid {
            overflow: self.badness(width).0 as Overflow,
            lines: self.lines,
        }
    }

    fn layouts<A: Annotation>(mut self, doc: &Doc<A>, indent: Option<usize>) -> Vec<Layout> {
        use Notation::*;

        match doc.notation() {
            Empty => vec![self],
            Text(text) if self.is_full && text.chars().count() > 0 => vec![],
            Text(text) => {
                self.lines.last_mut().unwrap().push_str(text);
                vec![self]
            }
            Spaces(n) if self.is_full && *n > 0 => vec![],
            Spaces(n) => {
                let last_line = self.lines.last_mut().unwrap();
                for _ in 0..*n {
                    last_line.push(' ');
                }
                vec![self]
            }
            Newline => {
                if let Some(indent) = indent {
                    self.lines.push(" ".repeat(indent as usize));
                    self.is_full = false;
                    vec![self]
                } else {
                    vec![]
                }
            }
            EndOfLine => {
                self.is_full = true;
                vec![self]
            }
            Indent(j, doc) => self.layouts(doc, indent.map(|i| i + (*j as usize))),
            Flat(doc) => self.layouts(doc, None),
            Align(doc) => {
                let ind = self.lines.last().unwrap().chars().count();
                self.layouts(doc, indent.map(|i| i + ind))
            }
            Concat(doc1, doc2) => self
                .layouts(doc1, indent)
                .into_iter()
                .flat_map(|lay1| lay1.layouts(doc2, indent))
                .collect::<Vec<_>>(),
            Choice(doc1, doc2) => {
                let mut layouts = self.clone().layouts(doc1, indent);
                layouts.append(&mut self.layouts(doc2, indent));
                layouts
            }
            Annotate(_, doc) => self.layouts(doc, indent),
        }
    }
}

fn has_same_lines(left: &PrettyResult, right: &PrettyResult) -> bool {
    match (left, right) {
        (
            PrettyResult::Valid {
                lines: left_lines, ..
            },
            PrettyResult::Valid {
                lines: right_lines, ..
            },
        ) => left_lines == right_lines,
        (_, _) => false,
    }
}
