use crate::line_lens::LastLineLens;
use crate::node::Width;
use std::fmt;

// INVARIANT: prefix <= width && suffix <= width
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AvailableSpace {
    Flat(Width),
    NotFlat {
        /// Length of extra stuff at the start of the first line.
        prefix: Width,
        /// Width avilable for all lines.
        width: Width,
        /// Length of extra stuff at the end of the last line.
        suffix: Width,
    },
}

impl fmt::Display for AvailableSpace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AvailableSpace::*;

        match self {
            Flat(w) => write!(f, "Flat({})", w),
            NotFlat {
                prefix,
                width,
                suffix,
            } => write!(f, "{}:{}:{}", prefix, width, suffix),
        }
    }
}

impl AvailableSpace {
    pub fn new_rectangle(width: Width) -> AvailableSpace {
        AvailableSpace::NotFlat {
            prefix: 0,
            width,
            suffix: 0,
        }
    }

    pub fn fits_single_line(self, len: Width) -> bool {
        use AvailableSpace::*;

        match self {
            Flat(w) => len <= w,
            NotFlat {
                prefix,
                width,
                suffix,
            } => prefix + len + suffix <= width,
        }
    }

    pub fn flatten(self) -> Option<AvailableSpace> {
        use AvailableSpace::*;

        match self {
            Flat(w) => Some(Flat(w)),
            NotFlat {
                prefix,
                width,
                suffix,
            } => {
                if prefix + suffix <= width {
                    Some(Flat(width - prefix - suffix))
                } else {
                    None
                }
            }
        }
    }

    pub fn align(self) -> AvailableSpace {
        use AvailableSpace::*;

        match self {
            Flat(w) => Flat(w),
            NotFlat {
                prefix,
                width,
                suffix,
            } => NotFlat {
                prefix: 0,
                width: width - prefix,
                suffix,
            },
        }
    }

    pub fn splits(
        self,
        lens: LastLineLens,
    ) -> impl Iterator<Item = (AvailableSpace, AvailableSpace)> {
        let single_spaces = lens
            .single
            .into_iter()
            .filter_map(move |len| self.split_single(len));
        let aligned_spaces = lens
            .aligned
            .into_iter()
            .filter_map(move |len| self.split_aligned(len));
        let multi_spaces = lens
            .multi
            .into_iter()
            .filter_map(move |len| self.split_multi(len));
        single_spaces.chain(aligned_spaces).chain(multi_spaces)
    }

    fn split_single(self, len: Width) -> Option<(AvailableSpace, AvailableSpace)> {
        use AvailableSpace::*;

        match self {
            Flat(width) => {
                if len > width {
                    return None;
                }
                Some((Flat(len), Flat(width - len)))
            }
            NotFlat {
                prefix,
                width,
                suffix,
            } => {
                if prefix + len > width {
                    return None;
                }
                let left = Flat(len);
                let right = NotFlat {
                    prefix: prefix + len,
                    width: width,
                    suffix: suffix,
                };
                Some((left, right))
            }
        }
    }

    fn split_multi(self, len: Width) -> Option<(AvailableSpace, AvailableSpace)> {
        use AvailableSpace::*;

        match self {
            Flat(_) => None,
            NotFlat {
                prefix,
                width,
                suffix,
            } => {
                if len > width {
                    return None;
                }
                let left = NotFlat {
                    prefix: prefix,
                    width: width,
                    suffix: width - len,
                };
                let right = NotFlat {
                    prefix: len,
                    width: width,
                    suffix: suffix,
                };
                Some((left, right))
            }
        }
    }

    fn split_aligned(self, len: Width) -> Option<(AvailableSpace, AvailableSpace)> {
        use AvailableSpace::*;

        match self {
            Flat(_) => None,
            NotFlat {
                prefix,
                width,
                suffix,
            } => {
                if prefix + len > width {
                    return None;
                }
                let left = NotFlat {
                    prefix: 0,
                    width: width - prefix,
                    suffix: width - prefix - len,
                };
                let right = NotFlat {
                    prefix: prefix + len,
                    width: width,
                    suffix: suffix,
                };
                Some((left, right))
            }
        }
    }
}
