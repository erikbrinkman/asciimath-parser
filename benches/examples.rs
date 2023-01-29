use asciimath_parser::ASCIIMATH_TOKENS;
use lazy_static::lazy_static;
use rand::distributions::{Alphanumeric, Slice};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use rand_distr::WeightedIndex;

pub const EXAMPLES: [&'static str; 1] = ["sum_(i=1)^n i^3=((n(n+1))/2)^2"];

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

lazy_static! {
    pub static ref RANDOM_EXAMPLES: [String; 30] = {
        let mut rng = StdRng::from_seed([0; 32]);
        [(); 30].map(|_| random_string(&mut rng, &ASCIIMATH_TOKENS))
    };
}
