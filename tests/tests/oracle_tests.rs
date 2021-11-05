use super::common::assert_unknown;
use super::common::{doc_factory, Size};
use prettiest::{Doc, Width};
use std::ops::Range;

const SAMPLE_COUNT: usize = 1000;
static WIDTHS: &[Width] = &[0, 1, 2, 4, 7, 10, 15];
const SIZES: Range<Size> = 4..20;

#[test]
fn oracular_testing() {
    let mut factory = doc_factory();
    println!("Running oracle tests");
    for size in 0..4 {
        let count = factory.count::<Doc<()>>(size).unwrap();
        println!("size: {} count: {}", size, count);
        for doc in factory.iter(size) {
            for width in WIDTHS {
                assert_unknown(&doc, *width);
            }
        }
    }
    for size in SIZES {
        println!("size: {} sample count: {}", size, SAMPLE_COUNT);
        println!(
            "random sample: {}",
            factory.random::<Doc<()>>(size).unwrap()
        );
        for _ in 0..SAMPLE_COUNT {
            let doc = factory.random(size).unwrap();
            for width in WIDTHS {
                assert_unknown(&doc, *width);
            }
        }
    }
}
