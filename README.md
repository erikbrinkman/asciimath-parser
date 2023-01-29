Asciimath Parser
================

A fast extensible memory-efficient asciimath parser

This parser produces a parsed tree representation rooted as an `Expression`.
The parsed structure keeps refrences to the underlying string in order to avoid
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

### Comparisons

This library is meant to be a fast extensible parser. There are a number of rust libraries that
parse and format, or parse and evaluate, but don't expose their underlying parsing logic. Only
`asciimath_rs` actual parses expressions. However, that parser allocates extra strings, and
produces a relatively complicated parse tree. This creates a relatively simpler parse tree with
string slices as tokens. This allows this parser be several times faster than `asciimath_rs`.

```
test asciimath_parser::example ... bench:       7,912 ns/iter (+/- 1,348)
test asciimath_rs::example     ... bench:      41,605 ns/iter (+/- 14,262)

test asciimath_parser::random  ... bench:     360,495 ns/iter (+/- 32,231)
test asciimath_rs::random      ... bench:   2,522,810 ns/iter (+/- 168,133)
```
