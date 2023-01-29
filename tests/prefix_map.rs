#[cfg(feature = "fst")]
use asciimath_parser::prefix_map::FstPrefixMap;
#[cfg(feature = "qp-trie")]
use asciimath_parser::prefix_map::QpTriePrefixMap;
use asciimath_parser::prefix_map::{HashPrefixMap, LinearPrefixMap};
use asciimath_parser::{Tokenizer, ASCIIMATH_TOKENS};
use rand::distributions::{Alphanumeric, Slice};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rand_distr::WeightedIndex;

fn random_string<V>(rng: &mut impl Rng, tokens: &[(&str, V)]) -> String {
    let token = Slice::new(tokens).unwrap();
    let choice = WeightedIndex::new([1, 1, 3]).unwrap();

    let mut res = String::new();
    for _ in 0..30 {
        match rng.sample(&choice) {
            0 => res.push(' '),
            1 => res.push(rng.sample(Alphanumeric).try_into().unwrap()),
            2 => res.push_str(rng.sample(&token).0),
            _ => unreachable!(),
        }
    }
    res
}

macro_rules! make_test {
    ($name:ident, $struct:ident, $factory:ident) => {
        mod $name {
            use super::*;

            #[test]
            fn random_prefix() {
                let linear_tokens = LinearPrefixMap::from_vec(ASCIIMATH_TOKENS);
                let ref_tokens = $struct::$factory(ASCIIMATH_TOKENS);

                let mut rng = StdRng::from_seed([0; 32]);
                for _ in 0..20 {
                    let string = random_string(&mut rng, &ASCIIMATH_TOKENS);
                    let mut linear = Tokenizer::with_tokens(&string, &linear_tokens, true);
                    let mut hash = Tokenizer::with_tokens(&string, &ref_tokens, true);
                    loop {
                        match (linear.next(), hash.next()) {
                            (Some(left), Some(right)) => assert_eq!(left, right),
                            (Some(left), None) => panic!("test missing {left:?}"),
                            (None, Some(right)) => panic!("linear missing {right:?}"),
                            (None, None) => break,
                        }
                    }
                }
            }
        }
    };
}

make_test! {hash, HashPrefixMap, from_iter}
#[cfg(feature = "fst")]
make_test! {fst, FstPrefixMap, from_vec}
#[cfg(feature = "qp-trie")]
make_test! {qptrie, QpTriePrefixMap, from_iter}
