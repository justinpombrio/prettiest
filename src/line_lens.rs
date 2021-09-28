use crate::node::{Width, MAX_WIDTH};

#[derive(Debug, Clone)]
pub struct LastLineLens {
    /// The lengths of all possible single-line layouts (containing no newlines).
    pub single: Vec<Width>,
    /// The lengths of the last lines of all possible multi-line aligned layouts. (Contains at
    /// least one newline, and the last line is aligned with the first line.)
    pub aligned: Vec<Width>,
    /// The lenghts of the last lines of all possible multi-line non-aligned layouts. (Contains at
    /// least one newline, and the last line is not aligned with the first line.)
    pub multi: Vec<Width>,
}

impl LastLineLens {
    pub fn new_single(width: Width) -> LastLineLens {
        LastLineLens {
            single: vec![width],
            aligned: vec![],
            multi: vec![],
        }
    }

    pub fn new_multi(width: Width) -> LastLineLens {
        LastLineLens {
            single: vec![],
            aligned: vec![],
            multi: vec![width],
        }
    }

    pub fn align(mut self) -> LastLineLens {
        self.aligned = self
            .aligned
            .into_iter()
            .chain(self.multi.into_iter())
            .collect();
        self.multi = vec![];
        self
    }

    pub fn flatten(mut self) -> LastLineLens {
        self.aligned.clear();
        self.multi.clear();
        self
    }

    pub fn union(self, other: LastLineLens) -> LastLineLens {
        // TODO: do an efficient merge-sort style merge, since these lists are ordered
        let single = self
            .single
            .into_iter()
            .chain(other.single.into_iter())
            .collect();
        let aligned = self
            .aligned
            .into_iter()
            .chain(other.aligned.into_iter())
            .collect();
        let multi = self
            .multi
            .into_iter()
            .chain(other.multi.into_iter())
            .collect();
        LastLineLens {
            single,
            aligned,
            multi,
        }
    }

    pub fn concat(self, other: LastLineLens) -> LastLineLens {
        let mut single = vec![];
        let mut aligned = vec![];
        let mut multi = vec![];

        for m in &self.single {
            for n in &other.single {
                single.push(*m + *n);
            }
            for n in &other.aligned {
                aligned.push(*m + *n);
            }
            for n in &other.multi {
                multi.push(*n);
            }
        }
        for m in &self.aligned {
            for n in &other.single {
                aligned.push(*m + *n);
            }
            for n in &other.aligned {
                aligned.push(*m + *n);
            }
            for n in &other.multi {
                multi.push(*n);
            }
        }
        for m in &self.multi {
            for n in &other.single {
                multi.push(*m + *n);
            }
            for n in &other.aligned {
                multi.push(*m + *n);
            }
            for n in &other.multi {
                multi.push(*n);
            }
        }
        single = normalize(single);
        aligned = normalize(aligned);
        multi = normalize(multi);

        LastLineLens {
            single,
            aligned,
            multi,
        }
    }

    pub fn iter_all(&self) -> impl Iterator<Item = Width> + '_ {
        let single = self.single.iter();
        let aligned = self.aligned.iter();
        let multi = self.multi.iter();

        single.chain(aligned).chain(multi).map(|n| *n)
    }
}

/// Normalize a list of widths: sort it, and filter out unreasonably large values.
fn normalize(mut list: Vec<Width>) -> Vec<Width> {
    list = list
        .into_iter()
        .filter(|n| *n <= MAX_WIDTH)
        .collect::<Vec<_>>();
    list.sort();
    list
}
