use asciimath_parser::{ASCIIMATH_TOKENS, parse};
use rand::distr::slice::Choose;
use rand::distr::weighted::WeightedIndex;
use rand::rngs::StdRng;
use rand::{Rng, RngExt, SeedableRng};
use std::hint::black_box;
use std::time::{Duration, Instant};

// opens outnumber closes so failed groups nest deeply, which is where backtracking can blow up
const OPENS: [&str; 5] = ["(", "[", "{", "|", "||"];
const STRUCTURE: [&str; 11] = [
    ")", "]", "}", ",", "^", "_", "/", "-", "sqrt", "frac", "text",
];

fn random_input(rng: &mut impl Rng, len: usize) -> String {
    let opens = Choose::new(&OPENS).unwrap();
    let structure = Choose::new(&STRUCTURE).unwrap();
    let token = Choose::new(&ASCIIMATH_TOKENS).unwrap();
    let choice = WeightedIndex::new([6, 3, 2, 1, 1]).unwrap();
    let mut input = String::new();
    for _ in 0..len {
        match rng.sample(&choice) {
            0 => input.push_str(rng.sample(opens)),
            1 => input.push_str(rng.sample(structure)),
            2 => input.push_str(rng.sample(token).0),
            3 => input.push(rng.random_range('a'..='z')),
            4 => input.push(' '),
            _ => unreachable!(),
        }
    }
    input
}

fn time_parse(input: &str) -> Duration {
    let start = Instant::now();
    black_box(parse(input));
    start.elapsed()
}

#[test]
fn random_inputs_parse_quickly() {
    let mut rng = StdRng::from_seed([0; 32]);
    black_box(parse("")); // build the default tokens outside the timing
    // typical inputs take about 1ms; exponential backtracking takes seconds
    let budget = Duration::from_millis(100);
    for _ in 0..2000 {
        let len = rng.random_range(1..=200);
        let input = random_input(&mut rng, len);
        // retime once so a scheduler hiccup doesn't fail the test
        let elapsed = time_parse(&input).min(time_parse(&input));
        assert!(elapsed < budget, "{input:?} took {elapsed:?}");
    }
}
