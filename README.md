Asciimath Parser
================
[![crates.io](https://img.shields.io/crates/v/asciimath-parser)](https://crates.io/crates/asciimath-parser)
[![docs](https://docs.rs/asciimath-parser/badge.svg)](https://docs.rs/asciimath-parser)
[![license](https://img.shields.io/github/license/erikbrinkman/asciimath-parser)](LICENSE)
[![tests](https://github.com/erikbrinkman/asciimath-parser/actions/workflows/rust.yml/badge.svg)](https://github.com/erikbrinkman/asciimath-parser/actions/workflows/rust.yml)

A fast extensible memory-efficient asciimath parser

This parser produces a parsed tree representation rooted as an `Expression`.
The parsed structure keeps references to the underlying string in order to avoid
copies, but these strings must still be interpreted as the correct tokens to
use the structure.

## Usage

```sh
cargo add asciimath-parser
```

then

```
asciimath_parser::parse("x / y");
```

### Performance

This library is meant to be a fast extensible parser. Most other rust libraries parse and format,
or parse and evaluate, but don't expose their underlying parsing logic. This parser produces a
relatively simple parse tree whose tokens are slices of the input, so it does no extra string
allocation. The `parse` bench measures parsing a corpus of hand-written examples and a batch of
random expressions:

```
test example ... bench:       2,232 ns/iter (+/- 115)
test random  ... bench:     113,775 ns/iter (+/- 6,033)
```
