use crate::line_lens::LastLineLens;
use crate::node::{Height, Id, Node, Notation, Width};
use crate::space::AvailableSpace;
use crate::{log, log_span};

use std::collections::HashMap;

pub fn pretty(node: &Node, width: Width) -> Option<Vec<String>> {
    let mut printer = Printer::new();
    let space = AvailableSpace::new_rectangle(width);
    printer.render(node, space)
}

struct Printer {
    measurement_cache: HashMap<(Id, AvailableSpace), Option<Height>>,
    last_line_len_cache: HashMap<Id, LastLineLens>,
}

impl Printer {
    pub fn new() -> Printer {
        Printer {
            measurement_cache: HashMap::new(),
            last_line_len_cache: HashMap::new(),
        }
    }

    fn possible_last_line_lens(&mut self, node: &Node) -> LastLineLens {
        use Notation::*;

        log_span!();

        if let Some(lens) = self.last_line_len_cache.get(&node.id) {
            return lens.to_owned();
        }

        let result = match &*node.notation {
            Text(s) => LastLineLens::new_single(s.chars().count() as Width),
            Spaces(w) => LastLineLens::new_single(*w),
            Newline => LastLineLens::new_multi(0),
            Flat(node) => self.possible_last_line_lens(node).flatten(),
            Align(node) => self.possible_last_line_lens(node).align(),
            Choice(opt1, opt2) => {
                let lens1 = self.possible_last_line_lens(opt1);
                let lens2 = self.possible_last_line_lens(opt2);
                lens1.union(lens2)
            }
            Concat(left, right) => {
                let lens1 = self.possible_last_line_lens(left);
                let lens2 = self.possible_last_line_lens(right);
                lens1.concat(lens2)
            }
        };

        self.last_line_len_cache.insert(node.id, result.clone());
        log!("Last lens={} n={:?}", result, node);
        result
    }

    fn measure(&mut self, node: &Node, space: AvailableSpace) -> Option<Height> {
        use Notation::*;

        log_span!();

        if let Some(height) = self.measurement_cache.get(&(node.id, space)) {
            return *height;
        }

        // INVARIANT: must match the behavior of `render`
        let result_height = match &*node.notation {
            Spaces(w) if space.fits_single_line(*w) => Some(0),
            Spaces(_) => None,
            Text(s) if space.fits_single_line(s.chars().count() as Width) => Some(0),
            Text(_) => None,
            Newline => Some(1),
            Flat(node) => self.measure(node, space.flatten()?),
            Align(node) => self.measure(node, space.align()),
            Choice(opt1, opt2) => {
                let h1 = self.measure(opt1, space);
                let h2 = self.measure(opt2, space);
                match (h1, h2) {
                    (None, None) => None,
                    (None, Some(h)) => Some(h),
                    (Some(h), None) => Some(h),
                    (Some(h1), Some(h2)) => Some(h1.min(h2)),
                }
            }
            Concat(left, right) => {
                let mut min_height: Option<Height> = None;
                let lens = self.possible_last_line_lens(left);
                for (left_space, right_space) in space.splits(lens) {
                    if let Some(h1) = self.measure(left, left_space) {
                        if let Some(h2) = self.measure(right, right_space) {
                            min_height = match min_height {
                                None => Some(h1 + h2),
                                Some(h) => Some(h.min(h1 + h2)),
                            };
                        }
                    }
                }
                min_height
            }
        };

        self.measurement_cache
            .insert((node.id, space), result_height);
        if let Some(h) = result_height {
            log!("Measure h={} space={} n={:?}", h, space, node);
        } else {
            log!("Measure h=NA space={} n={:?}", space, node);
        }
        result_height
    }

    fn render(&mut self, node: &Node, space: AvailableSpace) -> Option<Vec<String>> {
        use Notation::*;

        // INVARIANT: must match the behavior of `measure`
        match &*node.notation {
            Spaces(w) if space.fits_single_line(*w) => Some(vec![" ".repeat(*w as usize)]),
            Spaces(_) => None,
            Text(s) if space.fits_single_line(s.chars().count() as Width) => {
                Some(vec![s.to_owned()])
            }
            Text(_) => None,
            Newline => Some(vec!["".to_owned(), "".to_owned()]),
            Flat(node) => self.render(node, space.flatten()?),
            Align(node) => self.render(node, space.align()),
            Choice(opt1, opt2) => {
                let h1 = self.measure(opt1, space);
                let h2 = self.measure(opt2, space);
                match (h1, h2) {
                    (None, None) => None,
                    (None, Some(_)) => self.render(opt2, space),
                    (Some(_), None) => self.render(opt1, space),
                    (Some(h1), Some(h2)) if h1 <= h2 => self.render(opt1, space),
                    (Some(_), Some(_)) => self.render(opt2, space),
                }
            }
            Concat(left, right) => {
                // TODO: this logic gets a bit easier if `Option<Height>` becomes a specialized
                // enum.
                let mut best: Option<(AvailableSpace, AvailableSpace, Height)> = None;
                let lens = self.possible_last_line_lens(left);
                for (left_space, right_space) in space.splits(lens) {
                    if let Some(h1) = self.measure(left, left_space) {
                        if let Some(h2) = self.measure(right, right_space) {
                            best = match best {
                                None => Some((left_space, right_space, h1 + h2)),
                                Some((_, _, h)) if h <= h1 + h2 => best,
                                Some(_) => Some((left_space, right_space, h1 + h2)),
                            };
                        }
                    }
                }
                if let Some((left_space, right_space, _h)) = best {
                    let left_lines = self.render(left, left_space)?;
                    let right_lines = self.render(right, right_space)?;
                    Some(concat_lines(left_lines, right_lines))
                } else {
                    None
                }
            }
        }
    }
}

fn concat_lines(left_lines: Vec<String>, right_lines: Vec<String>) -> Vec<String> {
    let mut lines = left_lines;
    let mut right_iter = right_lines.into_iter();
    let middle_line = lines.pop().unwrap() + &right_iter.next().unwrap();
    lines.push(middle_line);
    for line in right_iter {
        lines.push(line);
    }
    lines
}
