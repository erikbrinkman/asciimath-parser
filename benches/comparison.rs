#![feature(test)]

mod examples;

extern crate test;

use ::asciimath_parser::parse as amp_parse;
use ::asciimath_rs::parse as amrs_parse;
use examples::{EXAMPLES, RANDOM_EXAMPLES};
use std::hint::black_box;
use test::Bencher;

macro_rules! make_bench {
    ($name:ident, $parse:ident) => {
        mod $name {
            use super::*;

            #[bench]
            fn example(bench: &mut Bencher) {
                bench.iter(|| {
                    for example in EXAMPLES {
                        black_box($parse(black_box(example)));
                    }
                });
            }

            #[bench]
            fn random(bench: &mut Bencher) {
                let examples = &*RANDOM_EXAMPLES; // deref to for generation outside of bench
                bench.iter(|| {
                    for example in examples {
                        black_box($parse(black_box(example)));
                    }
                });
            }
        }
    };
}

make_bench! {asciimath_parser, amp_parse}
make_bench! {asciimath_rs, amrs_parse}
