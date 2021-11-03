use super::common::assert_unknown;
use super::common::doc_factory;
use prettiest::{Doc, Width};

const SAMPLE_COUNT: usize = 100_000;
static WIDTHS: &[Width] = &[0, 1, 2, 4, 7, 10, 15];

#[test]
fn oracular_testing() {
    let mut factory = doc_factory();
    println!("Running oracle tests");
    for size in 0..4 {
        let count = factory.count::<Doc<()>>(size);
        println!("size: {} count: {}", size, count);
        for doc in factory.iter(size) {
            for width in WIDTHS {
                assert_unknown(&doc, *width);
            }
        }
    }
    for size in 4..20 {
        println!("size: {} sample count: {}", size, SAMPLE_COUNT);
        for _ in 0..SAMPLE_COUNT {
            let doc = factory.random(size).unwrap();
            for width in WIDTHS {
                assert_unknown(&doc, *width);
            }
        }
    }
}
