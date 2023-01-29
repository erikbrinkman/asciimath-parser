//! PrefixMaps are string keyed maps that support finding values with a longest prefix
//!
//! They are used by tokenizers to find valid tokens by seeing if the prefix of the current string
//! maps to a known token. Since this is a large part of asciimath parsing, the efficient of these
//! maps is heavily linked to overall parsing time.
//!
//! In benchmarks on random-ish strings, a prefix map based off of `qp-trie` was the fastest for
//! parsing asciimath, but using an `fnv` backed [HashPrefixMap] was close behind.
//!
//! # Benchmarks
//!
//! ```txt
//! test qptrie::random_prefix  ... bench:      85,604 ns/iter (+/- 23,794)
//! test fnv::random_prefix     ... bench:     107,734 ns/iter (+/- 15,949)
//! test hash::random_prefix    ... bench:     163,859 ns/iter (+/- 14,000)
//! test linear::random_prefix  ... bench:     482,320 ns/iter (+/- 42,428)
//! test fst::random_prefix     ... bench:  22,297,830 ns/iter (+/- 820,369)
//! ```
//!
//! # Example
//!
//! ```
//! use asciimath_parser::prefix_map::LinearPrefixMap;
//! use asciimath_parser::{ASCIIMATH_TOKENS, Tokenizer, parse_tokens};
//!
//! let token_map = LinearPrefixMap::from_vec(ASCIIMATH_TOKENS);
//! let tokens = Tokenizer::with_tokens("sum_i x_i", &token_map, true);
//! let parsed = parse_tokens(tokens);
//! ```

#[cfg(feature = "fst")]
mod fst;
mod hash;
mod linear;
#[cfg(feature = "qp-trie")]
mod trie;

#[cfg(feature = "fst")]
pub use self::fst::FstPrefixMap;
#[cfg(feature = "fnv")]
use ::fnv::FnvBuildHasher;
pub use hash::HashPrefixMap;
pub use linear::LinearPrefixMap;
#[cfg(feature = "qp-trie")]
pub use trie::QpTriePrefixMap;

/// A hash prefix map using the fnv hasher
///
/// This is the [second fastest tokenizer][crate::prefix_map], but requires the `fnv` feature.
///
/// # Example
/// ```
/// use asciimath_parser::prefix_map::FnvHashPrefixMap;
/// use asciimath_parser::{ASCIIMATH_TOKENS};
///
/// let token_map = FnvHashPrefixMap::from_iter_hasher(ASCIIMATH_TOKENS);
/// ```
#[cfg(feature = "fnv")]
pub type FnvHashPrefixMap<K, V> = HashPrefixMap<K, V, FnvBuildHasher>;

/// A PrefixMap is a map that supports operations on the prefix of an input
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
    fn get_longest_prefix<P: AsRef<str>>(&self, inp: P) -> Option<(usize, &V)>;
}

/// From a vec-like, order all the entries and remove duplicates
fn remove_ordered_dups<K, V>(inp: &mut Vec<(K, V)>)
where
    K: Eq,
{
    // NOTE this can potentially be achieved better with drain_filter once it's stabalized
    if let Some((ref first, _)) = inp.first() {
        let mut last = first;
        let mut off = 0;
        for ind in 1..inp.len() {
            if &inp[ind].0 == last {
                off += 1;
            }
            inp.swap(ind - off, ind);
            last = &inp[ind - off].0;
        }
        inp.truncate(inp.len() - off);
    }
}
