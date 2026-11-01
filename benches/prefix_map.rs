#![feature(test)]

mod examples;

extern crate test;

use asciimath_parser::prefix_map::HashPrefixMap;
use asciimath_parser::{Tokenizer, asciimath_spellings};
use examples::{EXAMPLES, RANDOM_EXAMPLES};
use std::hint::black_box;
use test::Bencher;

macro_rules! make_bench {
    ($name:ident, $struct:ident, $factory:ident) => {
        mod $name {
            use super::*;

            #[bench]
            fn example_prefix(bench: &mut Bencher) {
                let tokens = $struct::$factory(asciimath_spellings());
                bench.iter(|| {
                    for example in EXAMPLES {
                        for token in Tokenizer::with_tokens(black_box(example), &tokens, true) {
                            black_box(token);
                        }
                    }
                });
            }

            #[bench]
            fn random_prefix(bench: &mut Bencher) {
                let tokens = $struct::$factory(asciimath_spellings());
                let examples = &*RANDOM_EXAMPLES; // deref to for generation outside of bench
                bench.iter(|| {
                    for example in examples {
                        for token in Tokenizer::with_tokens(black_box(example), &tokens, true) {
                            black_box(token);
                        }
                    }
                });
            }
        }
    };
}

make_bench! {hash, HashPrefixMap, from_iter}
