use asciimath_parser::prefix_map::{HashPrefixMap, PrefixMap};
use asciimath_parser::{ASCIIMATH_TOKENS, Token, Tokenizer};
use rand::distr::Alphanumeric;
use rand::distr::slice::Choose;
use rand::distr::weighted::WeightedIndex;
use rand::rngs::StdRng;
use rand::{Rng, RngExt, SeedableRng};

/// A reference prefix map that checks every key
struct LinearPrefixMap(Vec<(&'static str, Token)>);

impl PrefixMap<Token> for LinearPrefixMap {
    fn get_longest_prefix(&self, inp: &str) -> Option<(usize, &Token)> {
        self.0
            .iter()
            .filter(|(key, _)| inp.starts_with(key))
            .max_by_key(|(key, _)| key.len())
            .map(|(key, token)| (key.len(), token))
    }
}

fn random_string<V>(rng: &mut impl Rng, tokens: &[(&str, V)]) -> String {
    let token = Choose::new(tokens).unwrap();
    let choice = WeightedIndex::new([1, 1, 3]).unwrap();

    let mut res = String::new();
    for _ in 0..30 {
        match rng.sample(&choice) {
            0 => res.push(' '),
            1 => res.push(rng.sample(Alphanumeric).into()),
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
                let linear_tokens = LinearPrefixMap(ASCIIMATH_TOKENS.into());
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
