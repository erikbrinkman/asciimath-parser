//! A fast extensible memory-efficient asciimath parser
//!
//! This parser produces a parsed tree representation rooted as an
//! [`Expression`][tree::Expression]. The parsed structure keeps refrences to the underlying string
//! in order to avoid copies, but these strings must still be interpreted as the correct tokens to
//! use the structure.
//!
//! ## Usage
//!
//! ```sh
//! cargo add asciimath-parser
//! ```
//!
//! then
//!
//! ```
//! asciimath_parser::parse("x / y");
//! ```
//!
//! ### Comparisons
//!
//! This library is meant to be a fast extensible parser. There are a number of rust libraries that
//! parse and format, or parse and evaluate, but don't expose their underlying parsing logic. Only
//! `asciimath_rs` actual parses expressions. However, that parser allocates extra strings, and
//! produces a relatively complicated parse tree. This creates a relatively simpler parse tree with
//! string slices as tokens. This allows this parser be several times faster than `asciimath_rs`.
//!
//! ```txt
//! test asciimath_parser::example ... bench:       7,912 ns/iter (+/- 1,348)
//! test asciimath_rs::example     ... bench:      41,605 ns/iter (+/- 14,262)
//!
//! test asciimath_parser::random  ... bench:     360,495 ns/iter (+/- 32,231)
//! test asciimath_rs::random      ... bench:   2,522,810 ns/iter (+/- 168,133)
//! ```
//!
//! ## Dialect
//!
//! Asciimath is a loose standard that aims for fault-tolerant parsing while looking close to what
//! you might type in ascii if you were trying. However, other than the [current buggy
//! implementation](https://github.com/asciimath/asciimathml/blob/master/ASCIIMathML.js), there's
//! no parse standard.
//!
//! The parsing is written manually, so it doesn't quite conform to this grammar, (which is also
//! very ambiguous), but this grammar is close to the way asciimath actually interprets strings. In
//! asciimath, left-right brackets have the highest precidence and almost any argument can be
//! [missing][tree::Simple::Missing], save the first.
//!
//! ```txt
//! v ::= any char | greek letters | numbers | ... | missing
//! u ::= sqrt | text | bb | ...               unary symbols for font commands
//! f ::= sin | cos | ...                      function symbols
//! b ::= frac | root | stackrel | ...         binary symbols
//! l ::= ( | [ | { | (: | {: | ...            left brackets
//! r ::= ) | ] | } | :) | :} | ...            right brackets
//! d ::= '|' | '||'                           left-right brackets
//! R ::= E | E,R                              Matrix row expression
//! M ::= lRr | lRr,M                          Matrix expression
//! S ::= v | lEr | uS | fS | bSS | dEd | lMr  Simple expression
//! P ::= _S | ^S | _S^S                       Power expressiong
//! I ::= fP?I | SP?                           Intermediate expression
//! E ::= IE | I/I                             Expression
//! ```
//!
//! Left-right brackets are closed greedily, and must match the same string on both sides. If they
//! can't be matched they'll be parsed as a symbol. This is particularly useful for probabilitiy
//! conditioning, e.g. "p(x|y)". For matrices, all left brackets must match, all right brackets
//! must match, the number of seperators (,) in each row must match, and there needs to be more
//! than one element. This is more narrow than asciimath, but prevents need to have hardcoded rules
//! for the difference between a set and a matrix.
//!
//! This dialect results in many ways to parse things that conceptually might have the same
//! meaning. `"raw test"` and `text(raw text)` might seem to have the same meaning, but the first
//! is actually parsed as raw text, and the second is parsed as a unary function "text" with an
//! argument. Similarly `1 / 2` and `frac 1 2` both represent the same thing, but the first is a
//! high level [`Frac`][tree::Frac] construct, while the later is a binary operator called "frac".
//!
//! ### Differences with Asciimath
//!
//! Asciimaths parsing of left-right brackets is confusing, in particular the default way they
//! handle expressions like ||x||. This library tokenizes "||" as one token and tries to match it
//! that way, which produces different results than asciimath. Additionally, asciimath will
//! sometimes put a phantom empty open brace if an expression ends on a "|". This proved difficult
//! to support and seemes like an unuseful edgecase as it could always be substituted with
//! "{: ...  :|".
//!
//! ### Extensions to Asciimath
//!
//! This parser is meant to be extensible, so if there are parts that don't function as desired,
//! they can be tweaked.
//!
//! 1. [`parse`][crate::parse()] uses the default tokenizer, but
//!    [`parse_tokens`][crate::parse_tokens] can be used to parse an iterator of tuples `(&str,
//!    Token)` for ant custom tokenization you write.
//! 2. Custom tokenizer options can be used by creating an alternate [`Tokenizer`] using
//!    [`with_tokens`][Tokenizer::with_tokens].
//!    ```
//!    use asciimath_parser::{parse_tokens, Tokenizer, ASCIIMATH_TOKENS};
//!    use asciimath_parser::prefix_map::HashPrefixMap;
//!
//!    let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
//!    let parsed = parse_tokens(Tokenizer::with_tokens("...", &token_map, false));
//!    ```
//! 3. Nonstandard tokens can be used instead by creating custom token maps:
//!    ```
//!    use asciimath_parser::{parse_tokens, Tokenizer, Token};
//!    use asciimath_parser::prefix_map::HashPrefixMap;
//!
//!    let token_map = HashPrefixMap::from_iter([
//!        ("@", Token::Symbol),
//!        // ...
//!    ]);
//!    let parsed = parse_tokens(Tokenizer::with_tokens("...", &token_map, true));
//!    ```
//!
//! ## Design
//!
//! This parser tries to balance a few different goals which mediate it's design:
//! 1. simple - The "standard" asciimath parser is complicated, makes several passes, is relatively
//!    difficult to tweak or modify, is error-prone, and produces somewhat inconsistent results. By
//!    making this parser as simple as possible all of those should be relatively easy.
//! 2. extensible - Asciimath isn't a standard and there's a lot about it that you might want to
//!    change, or add to suit a particular usecase.
//! 3. efficient - Fast and with as little memory as possible. Because the asciimath parse trees
//!    are trees, some heap allocation is necessary to store the recursive structure.
//!
//! As a result, this parser produces a parsed representation, but doesn't attach any meanings to
//! the tokens in the parsed tree. The default parser treats both "*" and "cdot" as tokens, but
//! doesn't say anywhere that they should be rendered the same. This choice was made so that you
//! could easily add or remove tokens, or even change their meaning, and this library doesn't have
//! to know.
//!
//! If you want to consume this output and make sure the tokens are parsed correctly, you can use
//! the exported const version of the tokens uses to parse. By default [`parse`][crate::parse()]
//! uses [crate::ASCIIMATH_TOKENS]
//!
//! ## Tree Structure
//!
//! The parsed representation is a tree like structure that has a hierarchy of types that roughly
//! follows [`Expression`][tree::Expression] -> [`Intermediate`][tree::Intermediate] ->
//! [`Frac`][tree::Frac] -> [`ScriptFunc`][tree::ScriptFunc] ->
//! [`SimpleScript`][tree::SimpleScript] / [`Func`][tree::Func] -> [`Simple`][tree::Simple]. The
//! exceptions to this hierarchy are [`Group`][tree::Group] and [`Matrix`][tree::Matrix] that are
//! both "simple" structures, but contain nested expressions. All of these types implement `From`
//! from their singleton children, allowing promoting simple types to more complex ones with
//! minimal overhead. All of their members are public allowing destrucutring, especially with the
//! `box_patterns` feature. See [`tree`][crate::tree] for more details.
//!
//! ```
//! use asciimath_parser::tree::{Expression, Simple};
//!
//! let expr = Expression::from_iter([Simple::Ident("x")]);
//! ```
//!
//! ### Manual creation
//!
//! Most the tree structures implement [From] any of their singular upstream components, and most
//! constructors support anything implementing [Into], meaning that you only need only need to
//! construct the lowest level argument, and it will get upcast to a higher tree structure as you
//! need it.
//!
//! For example:
//! ```
//! # use asciimath_parser::tree::{Expression, Simple};
//! let expr = Expression::from_iter([Simple::Ident("x")]);
//! ```
#![warn(missing_docs)]
mod parse;
pub mod prefix_map;
mod tokenizer;
pub mod tree;

pub use parse::{parse, parse_tokens};
pub use tokenizer::{Token, Tokenizer, ASCIIMATH_TOKENS};
