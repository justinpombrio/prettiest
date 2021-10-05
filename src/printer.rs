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
