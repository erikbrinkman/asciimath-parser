//! `PrefixMaps` are string keyed maps that support finding values with a longest prefix
//!
//! They are used by tokenizers to find valid tokens by seeing if the prefix of the current string
//! maps to a known token. Since this is a large part of asciimath parsing, the efficient of these
//! maps is heavily linked to overall parsing time.
//!
//! # Example
//!
//! ```
//! use asciimath_parser::prefix_map::HashPrefixMap;
//! use asciimath_parser::{ASCIIMATH_TOKENS, Tokenizer, parse_tokens};
//!
//! let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
//! let tokens = Tokenizer::with_tokens("sum_i x_i", &token_map, true);
//! let parsed = parse_tokens(tokens);
//! ```

mod hash;

pub use hash::HashPrefixMap;

/// A `PrefixMap` is a map that supports operations on the prefix of an input
pub trait PrefixMap<V> {
    /// Get the corresponding length and value of the key that is part of the lonest prefix of inp
    ///
    /// # Example
    /// ```
    /// use asciimath_parser::prefix_map::{HashPrefixMap, PrefixMap};
    ///
    /// let map = HashPrefixMap::from_iter([("a", 1), ("abc", 3)]);
    /// assert_eq!(map.get_longest_prefix("ab"), Some((1, &1)));
    /// ```
    fn get_longest_prefix(&self, inp: &str) -> Option<(usize, &V)>;
}
