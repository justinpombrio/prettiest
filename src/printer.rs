use crate::doc::{Annotation, Doc, Id, Notation, Width};
use crate::measure::{Measure, MeasureSet, Overflow, Space};
use std::collections::HashMap;

pub struct PrettyResult {
    pub lines: Vec<String>,
    pub overflow: Overflow,
}

pub fn pretty<A: Annotation>(doc: &Doc<A>, width: Width) -> PrettyResult {
    let space = Space::new_rectangle(width);

    let mut printer = Printer::new();
    let measures = printer.measure(doc, space);
    let best_measure = measures.best();
    let overflow = best_measure.overflow;

    let lines = printer.render(doc, space, best_measure, false, false);
    PrettyResult { lines, overflow }
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

        if let Some(measures) = self.cache.get(&(doc.id, space)) {
            return measures.clone();
        }

        let measures: MeasureSet = match doc.notation.as_ref() {
            Empty => MeasureSet::one_measure(Measure::single_line(0, space.first)),
            Text(text) => {
                let len = text.chars().count() as Width;
                MeasureSet::one_measure(Measure::single_line(len, space.first))
            }
            Spaces(len) => MeasureSet::one_measure(Measure::single_line(*len, space.first)),
            Newline => MeasureSet::one_measure(Measure::newline()),
            EndOfLine => {
                let mut measure = Measure::single_line(0, space.first);
                measure.is_full = true;
                MeasureSet::one_measure(measure)
            }
            Indent(ind, doc) => self.measure(doc, space.indent(*ind)),
            Flat(doc) => self.measure(doc, space).filter(|m| m.height == 0),
            Align(doc) => self.measure(doc, space.align()),
            Concat(doc1, doc2) => {
                let mut measures = MeasureSet::new();
                for m1 in self.measure(doc1, space) {
                    let remaining_space = space.consume(m1);
                    measures = measures.union(self.measure(doc2, remaining_space));
                }
                measures
            }
            Choice(doc1, doc2) => {
                let m1 = self.measure(doc1, space);
                let m2 = self.measure(doc2, space);
                m1.union(m2)
            }
            Annotate(_, doc) => self.measure(doc, space),
        };

        self.cache.insert((doc.id, space), measures.clone());
        measures
    }

    fn render<A: Annotation>(
        &mut self,
        doc: &Doc<A>,
        space: Space,
        measure: Measure,
        flattened: bool,
        aligned: bool,
    ) -> Vec<String> {
        use Notation::*;

        match doc.notation.as_ref() {
            Empty => MeasureSet::one_measure(Measure::single_line(0, space.first)),
            Text(text) => {
                let len = text.chars().count() as Width;
                MeasureSet::one_measure(Measure::single_line(len, space.first))
            }
            Spaces(len) => MeasureSet::one_measure(Measure::single_line(*len, space.first)),
            Newline => MeasureSet::one_measure(Measure::newline()),
            EndOfLine => {
                let mut measure = Measure::single_line(0, space.first);
                measure.is_full = true;
                MeasureSet::one_measure(measure)
            }
            Indent(ind, doc) => self.measure(doc, space.indent(*ind)),
            Flat(doc) => self.measure(doc, space).filter(|m| m.height == 0),
            Align(doc) => self.measure(doc, space.align()),
            Concat(doc1, doc2) => {
                let mut measures = MeasureSet::new();
                for m1 in self.measure(doc1, space) {
                    let remaining_space = space.consume(m1);
                    measures = measures.union(self.measure(doc2, remaining_space));
                }
                measures
            }
            Choice(doc1, doc2) => {
                let m1 = self.measure(doc1, space);
                let m2 = self.measure(doc2, space);
                m1.union(m2)
            }
            Annotate(_, doc) => self.measure(doc, space),
        };
    }
}

/*
use crate::node::{Id, Node, Notation, Width};
use crate::shapes::{Badness, Shape, ShapeSet, Space};
use crate::{log, log_span};

use std::collections::HashMap;

pub struct PrettyResult {
    pub lines: Vec<String>,
    pub badness: Badness,
}

pub fn pretty(node: &Node, width: Width) -> PrettyResult {
    let mut printer = Printer::new();

    let space = Space::new_rectangle(width);
    let shapes = printer.measure(node, space);
    let best_shape = shapes.best();
    let badness = best_shape.badness();

    let lines = printer.render(node, space, best_shape, false, false);
    PrettyResult { lines, badness }
}

struct Printer {
    cache: HashMap<(Id, Space), ShapeSet>,
}

impl Printer {
    fn new() -> Printer {
        Printer {
            cache: HashMap::new(),
        }
    }

    fn measure(&mut self, node: &Node, space: Space) -> ShapeSet {
        use Notation::*;

        log_span!();

        if let Some(shapes) = self.cache.get(&(node.id, space)) {
            log!(
                "Measure(cached) space={} shapes={} node={}",
                space,
                shapes,
                node
            );
            return shapes.clone();
        }

        // INVARIANT: must match the behavior of `render`
        let shapes = match &*node.notation {
            Spaces(len) => ShapeSet::one_shape(Shape::single_line(*len, space.first())),
            Text(s) => ShapeSet::one_shape(Shape::single_line(
                s.chars().count() as Width,
                space.first(),
            )),
            Newline => ShapeSet::one_shape(Shape::newline()),
            Flat(node) => self.measure(node, space.flatten()).flatten(),
            Align(node) => self.measure(node, space.align()).align(),
            Choice(opt1, opt2) => {
                let shapes1 = self.measure(opt1, space);
                let shapes2 = self.measure(opt2, space);
                shapes1.union(shapes2)
            }
            Concat(left, right) => {
                let mut shapes = ShapeSet::new();
                for left_shape in self.measure(left, space) {
                    let remaining_space = space.consume(left_shape);
                    for right_shape in self.measure(right, remaining_space) {
                        shapes.insert(left_shape.concat(right_shape));
                    }
                }
                shapes.normalize();
                shapes
            }
        };

        log!("Measure space={} shapes={} node={}", space, shapes, node);
        self.cache.insert((node.id, space), shapes.clone());
        shapes
    }

    fn render(
        &mut self,
        node: &Node,
        space: Space,
        shape: Shape,
        flattened: bool,
        aligned: bool,
    ) -> Vec<String> {
        use Notation::*;

        log_span!();
        log!("rendering {}", node);

        // INVARIANT: must match the behavior of `measure`
        let lines = match &*node.notation {
            Spaces(w) => vec![" ".repeat(*w as usize)],
            Text(s) => vec![s.to_owned()],
            Newline => vec!["".to_owned(), "".to_owned()],
            Flat(node) => self.render(node, space.flatten(), shape, true, aligned),
            Align(node) => self.render(node, space.align(), shape, flattened, true),
            Choice(opt1, opt2) => {
                if self.measure(opt1, space).contains(shape) {
                    self.render(opt1, space, shape, flattened, aligned)
                } else {
                    assert!(self.measure(opt2, space).contains(shape));
                    self.render(opt2, space, shape, flattened, aligned)
                }
            }
            Concat(left, right) => {
                for left_shape in self.measure(left, space) {
                    let remaining_space = space.consume(left_shape);
                    for right_shape in self.measure(right, remaining_space) {
                        let mut full_shape = left_shape.concat(right_shape);
                        if flattened {
                            full_shape = full_shape.flatten();
                        }
                        if aligned {
                            full_shape = full_shape.align();
                        }
                        if full_shape == shape {
                            let left_lines = self.render(left, space, left_shape, false, false);
                            let right_lines =
                                self.render(right, remaining_space, right_shape, false, false);
                            return concat_lines(left_lines, right_lines, right_shape.aligned());
                        }
                    }
                }
                panic!("render: concat failed to find appropriate measure");
            }
        };

        lines
    }
}

fn concat_lines(left_lines: Vec<String>, right_lines: Vec<String>, aligned: bool) -> Vec<String> {
    let mut lines = left_lines;
    let mut right_iter = right_lines.into_iter();
    let mut middle_line = lines.pop().unwrap();
    let offset = middle_line.chars().count();
    middle_line.push_str(&right_iter.next().unwrap());
    lines.push(middle_line);
    for line in right_iter {
        if aligned {
            lines.push(" ".repeat(offset) + &line);
        } else {
            lines.push(line);
        }
    }
    lines
}
*/
