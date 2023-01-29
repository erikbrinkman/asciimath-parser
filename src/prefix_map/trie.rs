use super::PrefixMap;
use qp_trie::Trie;
use std::borrow::Borrow;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Wrapper<K>(K);

impl<K> Borrow<[u8]> for Wrapper<K>
where
    K: Borrow<str>,
{
    fn borrow(&self) -> &[u8] {
        self.0.borrow().as_bytes()
    }
}

/// A prefix map backed by a qp-trie
///
/// This is the [fastest][crate::prefix_map] and default token map. This prefix map requires the `qp-trie`
/// feature (enabled by default). This takes time `O(longest_prefix)`.
///
/// # Example
/// ```
/// use asciimath_parser::prefix_map::QpTriePrefixMap;
/// use asciimath_parser::{ASCIIMATH_TOKENS};
///
/// let token_map = QpTriePrefixMap::from_iter(ASCIIMATH_TOKENS);
/// ```
#[derive(Debug, Clone)]
pub struct QpTriePrefixMap<K: Clone, V>(Trie<Wrapper<K>, V>);

impl<K, V> FromIterator<(K, V)> for QpTriePrefixMap<K, V>
where
    K: Borrow<str> + Clone,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        QpTriePrefixMap(
            iter.into_iter()
                .map(|(key, val)| (Wrapper(key), val))
                .collect(),
        )
    }
}

impl<K, V> PrefixMap<V> for QpTriePrefixMap<K, V>
where
    K: Borrow<str> + Clone,
{
    fn get_longest_prefix<P: AsRef<str>>(&self, inp: P) -> Option<(usize, &V)> {
        let bytes = inp.as_ref().as_bytes();
        let empty = &[][..];
        let mut subtrie = self.0.subtrie(empty);
        let mut res = subtrie.get(empty).map(|val| (0, val));
        for len in 1..=bytes.len() {
            let slice = &bytes[..len];
            subtrie = subtrie.subtrie(slice);
            if subtrie.is_empty() {
                break;
            } else if let Some(val) = subtrie.get(slice) {
                res = Some((len, val))
            }
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::{PrefixMap, QpTriePrefixMap};

    #[test]
    fn correct_prefixes() {
        let map = QpTriePrefixMap::from_iter([("a", 0), ("abc", 1), ("bc", 2), ("bc", 3)]);
        assert_eq!(map.get_longest_prefix("abcd"), Some((3, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((1, &0)));
        assert_eq!(map.get_longest_prefix("bcd"), Some((2, &3)));
        assert_eq!(map.get_longest_prefix("bd"), None);
        assert_eq!(map.get_longest_prefix("💖"), None);
    }

    #[test]
    fn works_for_perverse() {
        let map = QpTriePrefixMap::from_iter([("", 0), (" 3", 1)]);
        assert_eq!(map.get_longest_prefix(" 3 "), Some((2, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((0, &0)));
    }
}
