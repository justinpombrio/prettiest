use crate::doc::{Annotation, Doc, Id, Notation, Width};
use crate::measure::{Measure, MeasureSet, Overflow};
use crate::space::Space;
use crate::{log, log_span};
use std::collections::HashMap;

pub struct PrettyResult {
    pub lines: Vec<String>,
    pub overflow: Overflow,
}

pub fn pretty<A: Annotation>(doc: &Doc<A>, width: Width) -> Option<PrettyResult> {
    let space = Space::new_rectangle(width);

    let mut printer = Printer::new();
    let measures = printer.measure(doc, space);
    let best_measure = measures.best()?;
    let overflow = best_measure.overflow;

    let lines = printer.render(doc, space, best_measure).0;
    Some(PrettyResult { lines, overflow })
}

struct Printer {
    cache: HashMap<(Id, Space), MeasureSet>,
}

impl Printer {
    fn new() -> Printer {
        Printer {
            cache: HashMap::new(),
        }
    }

    fn measure<A: Annotation>(&mut self, doc: &Doc<A>, space: Space) -> MeasureSet {
        use Notation::*;

        log_span!();

        if let Some(measures) = self.cache.get(&(doc.id, space)) {
            log!("Measure (cached) ({}) {} = {}", doc, space, measures);
            return measures.clone();
        }

        // INVARIANT: Must match the behavior of `render`
        let measures: MeasureSet = match doc.notation.as_ref() {
            Empty => MeasureSet::one_measure(Measure::single_line(0, space.first)),
            Text(text) => {
                let len = text.chars().count() as Width;
                MeasureSet::one_measure(Measure::single_line(len, space.first))
            }
            Spaces(len) => MeasureSet::one_measure(Measure::single_line(*len, space.first)),
            Newline => match space.indent {
                None => MeasureSet::new(),
                Some(ind) => MeasureSet::one_measure(Measure::newline(ind, space.width)),
            },
            EndOfLine => {
                let mut measure = Measure::single_line(0, space.first);
                measure.is_full = true;
                MeasureSet::one_measure(measure)
            }
            Indent(ind, doc) => self.measure(doc, space.indent(*ind)),
            Flat(doc) => self.measure(doc, space.flatten()),
            Align(doc) => self.measure(doc, space.align()),
            Concat(doc1, doc2) => {
                let mut measures = MeasureSet::new();
                for m1 in self.measure(doc1, space) {
                    let remaining_space = space.consume(m1);
                    let ms2 = self.measure(doc2, remaining_space);
                    measures = measures.union(ms2.map(|m2| m1.concat(m2)));
                }
                measures
            }
            Choice(doc1, doc2) => {
                let ms1 = self.measure(doc1, space);
                let ms2 = self.measure(doc2, space);
                ms1.union(ms2)
            }
            Annotate(_, doc) => self.measure(doc, space),
        };

        log!("Measure ({}) {} = {}", doc, space, measures);
        self.cache.insert((doc.id, space), measures.clone());
        measures
    }

    fn render<A: Annotation>(&mut self, doc: &Doc<A>, space: Space, measure: Measure) -> Lines {
        use Notation::*;

        // INVARIANT: Must match the behavior of `measure`
        match doc.notation.as_ref() {
            Empty => Lines::new(),
            Spaces(len) => Lines::new().append(&" ".repeat(*len as usize)),
            Text(text) => Lines::new().append(text),
            Newline => {
                let indent = space.indent.unwrap() as usize;
                Lines::new().newline().append(&" ".repeat(indent))
            }
            EndOfLine => Lines::new(),
            Indent(ind, doc) => self.render(doc, space.indent(*ind), measure),
            Flat(doc) => self.render(doc, space.flatten(), measure),
            Align(doc) => self.render(doc, space.align(), measure),
            Choice(opt1, opt2) => {
                if self.measure(opt1, space).contains(measure) {
                    self.render(opt1, space, measure)
                } else {
                    assert!(self.measure(opt2, space).contains(measure));
                    self.render(opt2, space, measure)
                }
            }
            Concat(doc1, doc2) => {
                for m1 in self.measure(doc1, space) {
                    let remaining_space = space.consume(m1);
                    for m2 in self.measure(doc2, remaining_space) {
                        if m1.concat(m2) == measure {
                            let lines1 = self.render(doc1, space, m1);
                            let lines2 = self.render(doc2, remaining_space, m2);
                            return lines1.concat(lines2);
                        }
                    }
                }
                panic!("render: concat failed to find appropriate measure");
            }
            // TODO
            Annotate(_, doc) => self.render(doc, space, measure),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Lines(Vec<String>);

impl Lines {
    fn new() -> Lines {
        Lines(vec!["".to_owned()])
    }

    fn append(mut self, text: &str) -> Lines {
        self.0.last_mut().unwrap().push_str(text);
        self
    }

    fn newline(mut self) -> Lines {
        self.0.push("".to_owned());
        self
    }

    fn concat(mut self, other: Lines) -> Lines {
        let mut extra_lines = other.0.into_iter();
        self.0
            .last_mut()
            .unwrap()
            .push_str(&extra_lines.next().unwrap());
        self.0.extend(extra_lines);
        self
    }
}
