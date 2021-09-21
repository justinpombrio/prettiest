use std::collections::HashMap;

const MAX_WIDTH: Width = 256;

type Id = usize;
type Width = u16;
/// Number of newlines. A single line has height 0.
type Height = u32;

pub struct Node {
    id: Id,
    inner: Box<NodeInner>,
}

pub enum NodeInner {
    Text(String),
    Horz(Node, Node),
    Vert(Node, Node),
    Choice(Node, Node),
}
use NodeInner::*;

pub struct Printer {
    measure_cache: HashMap<(Id, Width, Width), Option<Height>>,
    indent_cache: HashMap<Id, Vec<Width>>,
}

impl Printer {
    pub fn new() -> Printer {
        Printer {
            measure_cache: HashMap::new(),
            indent_cache: HashMap::new(),
        }
    }

    fn possible_indents(&mut self, node: &Node) -> Vec<Width> {
        if let Some(inds) = self.indent_cache.get(&node.id) {
            return inds.clone();
        }

        let result = match &*node.inner {
            Text(s) => vec![s.chars().count() as Width],
            Choice(opt1, opt2) => {
                // TODO: do an efficient merge-sort style merge, since these lists are ordered
                let mut inds1 = self.possible_indents(opt1);
                let mut inds2 = self.possible_indents(opt2);
                inds1.append(&mut inds2);
                inds1
            }
            Horz(left, right) => {
                let inds1 = self.possible_indents(left);
                let inds2 = self.possible_indents(right);

                // Combine all pairs.
                let mut inds = vec![];
                for i in &inds1 {
                    for j in &inds2 {
                        inds.push(i + j);
                    }
                }

                // Normalize the list. (Sort it, and filter out unreasonably large values.)
                inds = inds
                    .into_iter()
                    .filter(|n| *n <= MAX_WIDTH)
                    .collect::<Vec<_>>();
                inds.sort();

                inds
            }
            Vert(_top, bot) => self.possible_indents(bot),
        };

        self.indent_cache.insert(node.id, result.clone());
        result
    }

    pub fn measure(&mut self, node: &Node, width: Width, last: Width) -> Option<Height> {
        if let Some(height) = self.measure_cache.get(&(node.id, width, last)) {
            return *height;
        }

        let result_height = match &*node.inner {
            Text(s) => {
                let len = s.chars().count();
                if len <= last as usize {
                    Some(0)
                } else {
                    None
                }
            }
            Choice(opt1, opt2) => {
                let h1 = self.measure(opt1, width, last);
                let h2 = self.measure(opt2, width, last);
                match (h1, h2) {
                    (None, None) => None,
                    (None, Some(h)) => Some(h),
                    (Some(h), None) => Some(h),
                    (Some(h1), Some(h2)) => Some(h1.min(h2)),
                }
            }
            Horz(left, right) => {
                let mut height = None;
                for ind in self.possible_indents(left) {
                    // TODO: only need to consider measurements with _exactly_ this indent.
                    let h1 = self.measure(left, width, ind);
                    let h2 = self.measure(right, width - ind, last - ind);
                    if let (Some(h1), Some(h2)) = (h1, h2) {
                        if let Some(h3) = height {
                            height = Some(h1.min(h2.min(h3)));
                        } else {
                            height = Some(h1.min(h2));
                        }
                    }
                }
                height
            }
            Vert(top, bot) => {
                let h1 = self.measure(top, width, width);
                let h2 = self.measure(bot, width, last);
                if let (Some(h1), Some(h2)) = (h1, h2) {
                    Some(h1 + h2 + 1)
                } else {
                    None
                }
            }
        };

        self.measure_cache
            .insert((node.id, width, last), result_height);
        result_height
    }
}
