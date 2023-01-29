use super::PrefixMap;
use std::borrow::Borrow;
use std::cmp::min;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash};

/// A prefix map that uses length buckets and hashmaps for lookups
///
/// For asciimath, the default hasher makes this the third fastest prefix map. If the `qp-trie`
/// feature isn't enabled, this will be the default. Finding the longest prefix takes
/// `O(len_search_key)`, but if all keys are long and roughly the same length this may be faster
/// than `QpTriePrefixMap`.
///
/// # Example
/// ```
/// use asciimath_parser::prefix_map::HashPrefixMap;
/// use asciimath_parser::{ASCIIMATH_TOKENS};
///
/// let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
/// ```
#[derive(Debug, Clone)]
pub struct HashPrefixMap<K, V, S = RandomState>(Box<[HashMap<K, V, S>]>);

impl<K, V, S> HashPrefixMap<K, V, S>
where
    K: Borrow<str> + Hash + Eq,
    S: BuildHasher + Default,
{
    /// Create from an iterator and custom hasher
    pub fn from_iter_hasher<T, I>(iter: T) -> Self
    where
        T: IntoIterator<IntoIter = I>,
        I: Iterator<Item = (K, V)>,
    {
        let mut bins = Vec::new();
        for (key, val) in iter {
            let len = key.borrow().len();
            bins.extend((bins.len()..=len).map(|_| HashMap::default()));
            bins[len].insert(key, val);
        }
        HashPrefixMap(bins.into())
    }
}

impl<K, V> FromIterator<(K, V)> for HashPrefixMap<K, V>
where
    K: Borrow<str> + Hash + Eq,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Self::from_iter_hasher(iter)
    }
}

impl<K, V, S> PrefixMap<V> for HashPrefixMap<K, V, S>
where
    K: Borrow<str> + Hash + Eq,
    S: BuildHasher,
{
    fn get_longest_prefix<P: AsRef<str>>(&self, inp: P) -> Option<(usize, &V)> {
        let inp = inp.as_ref();
        for (len, map) in self.0[..min(self.0.len(), inp.len() + 1)]
            .iter()
            .enumerate()
            .rev()
        {
            if inp.is_char_boundary(len) {
                if let Some(val) = map.get(&inp[..len]) {
                    return Some((len, val));
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{HashPrefixMap, PrefixMap};

    #[test]
    fn correct_prefixes() {
        let map = HashPrefixMap::from_iter([("a", 0), ("abc", 1), ("bc", 2), ("bc", 3)]);
        assert_eq!(map.get_longest_prefix("abcd"), Some((3, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((1, &0)));
        assert_eq!(map.get_longest_prefix("bcd"), Some((2, &3)));
        assert_eq!(map.get_longest_prefix("bd"), None);
        assert_eq!(map.get_longest_prefix("💖"), None);
    }

    #[test]
    fn works_for_perverse() {
        let map = HashPrefixMap::from_iter([("", 0), (" 3", 1)]);
        assert_eq!(map.get_longest_prefix(" 3 "), Some((2, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((0, &0)));
    }
}
