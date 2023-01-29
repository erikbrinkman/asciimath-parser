#![feature(test)]

mod examples;

extern crate test;

#[cfg(feature = "fnv")]
use asciimath_parser::prefix_map::FnvHashPrefixMap;
#[cfg(feature = "fst")]
use asciimath_parser::prefix_map::FstPrefixMap;
#[cfg(feature = "qp-trie")]
use asciimath_parser::prefix_map::QpTriePrefixMap;
use asciimath_parser::prefix_map::{HashPrefixMap, LinearPrefixMap};
use asciimath_parser::{Tokenizer, ASCIIMATH_TOKENS};
use examples::{EXAMPLES, RANDOM_EXAMPLES};
use std::hint::black_box;
use test::Bencher;

macro_rules! make_bench {
    ($name:ident, $struct:ident, $factory:ident) => {
        mod $name {
            use super::*;

            #[bench]
            fn example_prefix(bench: &mut Bencher) {
                let tokens = $struct::$factory(ASCIIMATH_TOKENS);
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
                let tokens = $struct::$factory(ASCIIMATH_TOKENS);
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

make_bench! {linear, LinearPrefixMap, from_vec}
make_bench! {hash, HashPrefixMap, from_iter}
#[cfg(feature = "fst")]
make_bench! {fst, FstPrefixMap, from_vec}
#[cfg(feature = "fnv")]
make_bench! {fnv, FnvHashPrefixMap, from_iter_hasher}
#[cfg(feature = "qp-trie")]
make_bench! {qptrie, QpTriePrefixMap, from_iter}
