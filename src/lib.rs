//! A fast extensible memory-efficient asciimath parser
//!
//! This parser produces a parsed tree representation rooted as an
//! [`Expression`][tree::Expression]. The parsed structure keeps references to the underlying string
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
//! ### Performance
//!
//! This library is meant to be a fast extensible parser. Most other rust libraries parse and
//! format, or parse and evaluate, but don't expose their underlying parsing logic. This parser
//! produces a relatively simple parse tree whose tokens are slices of the input, so it does no
//! extra string allocation. The `parse` bench measures parsing a corpus of hand-written examples
//! and a batch of random expressions:
//!
//! ```txt
//! test example ... bench:       2,232 ns/iter (+/- 115)
//! test random  ... bench:     113,775 ns/iter (+/- 6,033)
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
//! asciimath, left-right brackets have the highest precedence and almost any argument can be
//! [missing][tree::Simple::Missing], save the first.
//!
//! ```txt
//! v ::= any char | greek letters | numbers | ... | missing
//! u ::= sqrt | text | bb | ...               unary symbols for font commands
//! f ::= sin | cos | ...                      function symbols
//! b ::= frac | root | stackrel | ...         binary symbols
//! g ::= + | - | +- | pm | ...                signs
//! l ::= ( | [ | { | (: | {: | ...            left brackets
//! r ::= ) | ] | } | :) | :} | ...            right brackets
//! d ::= '|' | '||'                           left-right brackets
//! R ::= E | E,R                              Matrix row expression
//! M ::= lRr | lRr,M                          Matrix expression
//! S ::= v | g | gS | lEr | uS | fS | bSS | dEd | lMr  Simple expression
//! P ::= _S | ^S | _S^S                       Power expression
//! I ::= fP?I | SP? | gI                      Intermediate expression
//! E ::= IE | I/I                             Expression
//! ```
//!
//! Every symbol the token map defines arrives with a [class][SymbolClass] saying what it is, so
//! that whatever reads the tree can space it without keeping a table of spellings: whether it is
//! written as a run of letters, like `dx` or `mod`, which can't be written against another such run
//! without the two reading as one name; whether it joins the operands on either side of it, like
//! `=` or `xx`; whether it wants the operand after it, like `sum` or `lim`; whether it is space,
//! like `quad`; or whether it is the separator. An [`Ident`][tree::Simple::Ident] is then a bare
//! variable, a single character under the default tokenizer, and never a name. A big operator keeps
//! its scripts and leaves the operand after it as the next part of the expression, as it always
//! has, while `sin` and the other [functions][Token::Function] still take theirs.
//!
//! A [sign][Token::Sign] — `+`, `-`, or a spelling like `+-` or `pm` — either joins the operands
//! around it or prefixes the one after it, and the parse settles which. It prefixes what follows
//! exactly when what sits to its left isn't a complete operand: nothing at all, because the sign
//! starts the input, a group, or an argument; another operator or sign; or a symbol that wants an
//! operand of its own, which is anything that joins the operands around it, wants the one after it,
//! separates, or is space. Everything else to the left is a target it joins, so `x^2 - 1` subtracts
//! while `x = -y` and `sum -x` both prefix and `x^-1` has the superscript `-1`. A prefixing sign
//! comes out with its operand as one [`Signed`][tree::Signed] part, scripts included, so `-x^2` is
//! a single signed operand; a joining sign is a [`Sign`][tree::Simple::Sign] of its own.
//!
//! Left-right brackets are closed greedily, and must match the same string on both sides. If they
//! can't be matched they'll be parsed as a symbol. This is particularly useful for probability
//! conditioning, e.g. "p(x|y)". Matrices follow asciimath: any brackets, including `|`, can
//! surround comma-separated rows, every row must be bracketed by the same `(` `)` or `[` `]` pair
//! and have the same number of separators (,), and there needs to be more than one element. Rows of
//! `(` inside `{` `}` are a set of tuples rather than a matrix, so `{(x, y), (a, b)}` is a group
//! while `{:(x, y), (a, b):}` is a matrix. A column whose cells are all a lone `|`, e.g.
//! `[(a, |, b), (c, |, d)]`, is a [vertical line][tree::Matrix::column_lines] rather than a column.
//!
//! Whitespace only separates tokens, but the tree keeps it wherever it sat between two neighboring
//! intermediates of an expression, as a [`Space`][tree::Intermediate::Space], so that it can be
//! written back out. Whitespace anywhere else is dropped, like around the `/` of a fraction, before
//! a script or an argument, or just inside a bracket.
//!
//! This dialect results in many ways to parse things that conceptually might have the same
//! meaning. `"raw test"` and `text(raw text)` might seem to have the same meaning, but the first
//! is actually parsed as raw text, and the second is parsed as a unary function "text" whose
//! argument is a group holding the raw text. Similarly `1 / 2` and `frac 1 2` both represent the
//! same thing, but the first is a high level [`Frac`][tree::Frac] construct, while the later is a
//! binary operator called "frac".
//!
//! ### Differences with Asciimath
//!
//! Asciimaths parsing of left-right brackets is confusing, in particular the default way they
//! handle expressions like ||x||. This library tokenizes "||" as one token and tries to match it
//! that way, which produces different results than asciimath. Additionally, asciimath will
//! sometimes put a phantom empty open brace if an expression ends on a "|". This proved difficult
//! to support and seems like an unuseful edgecase as it could always be substituted with
//! "{: ...  :|".
//!
//! Asciimath makes the floor and ceiling marks `|__`, `__|`, `|~` and `~|`, and their spellings
//! `lfloor`, `rfloor`, `lceiling` and `rceiling`, plain symbols. Here they are brackets, so
//! `|__ x __|` is a [`Group`][tree::Group] like `(x)`.
//!
//! ### Extensions to Asciimath
//!
//! This parser is meant to be extensible, so if there are parts that don't function as desired,
//! they can be tweaked.
//!
//! 1. [`parse`][crate::parse()] uses the default tokenizer, but
//!    [`parse_tokens`] can be used to parse an iterator of tuples `(&str,
//!    Token)` for any custom tokenization you write.
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
//!    use asciimath_parser::{parse_tokens, SymbolClass, Tokenizer, Token};
//!    use asciimath_parser::prefix_map::HashPrefixMap;
//!
//!    let token_map = HashPrefixMap::from_iter([
//!        ("@", Token::Symbol(SymbolClass::Joining)),
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
//! uses [`crate::ASCIIMATH_TOKENS`]
//!
//! ## Tree Structure
//!
//! The parsed representation is a tree like structure that has a hierarchy of types that roughly
//! follows [`Expression`][tree::Expression] -> [`Intermediate`][tree::Intermediate] ->
//! [`Frac`][tree::Frac] -> [`ScriptFunc`][tree::ScriptFunc] ->
//! [`SimpleScript`][tree::SimpleScript] / [`Func`][tree::Func] / [`Signed`][tree::Signed] ->
//! [`Simple`][tree::Simple]. The exceptions to this hierarchy are [`Group`][tree::Group] and
//! [`Matrix`][tree::Matrix] that are both "simple" structures, but contain nested expressions. All
//! of these types implement `From` from their singleton children, allowing promoting simple types
//! to more complex ones with minimal overhead. Most of their members are public allowing
//! destructuring, especially with the `box_patterns` feature. See [`tree`] for more details.
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
#![forbid(unsafe_code)]
#![warn(clippy::pedantic, missing_docs)]

mod parse;
pub mod prefix_map;
mod tokenizer;
pub mod tree;

pub use parse::{parse, parse_tokens};
pub use tokenizer::{ASCIIMATH_TOKENS, SymbolClass, Token, Tokenizer};
