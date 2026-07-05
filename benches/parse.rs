#![feature(test)]

mod examples;

extern crate test;

use ::asciimath_parser::parse;
use examples::{EXAMPLES, RANDOM_EXAMPLES};
use std::hint::black_box;
use test::Bencher;

#[bench]
fn example(bench: &mut Bencher) {
    bench.iter(|| {
        for example in EXAMPLES {
            black_box(parse(black_box(example)));
        }
    });
}

#[bench]
fn random(bench: &mut Bencher) {
    let examples = &*RANDOM_EXAMPLES; // deref to for generation outside of bench
    bench.iter(|| {
        for example in examples {
            black_box(parse(black_box(example)));
        }
    });
}
