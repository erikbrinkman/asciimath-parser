use super::PrefixMap;
use std::borrow::Borrow;

/// A very simple prefix map
///
/// This prefix map stores prefix in descending order by length. While very simple, it was still
/// reasonably fast only requires keys with equality. This might be a good choice on machines with
/// limited memory especially if you're parsing a very small subset of asciimath. Finding the
/// longest prefix takes `O(num_prefixes)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinearPrefixMap<K, V>(Box<[(K, V)]>);

impl<K, V> LinearPrefixMap<K, V>
where
    K: Borrow<str> + Eq,
{
    /// Create from a vector of entries
    pub fn from_vec<B>(inp: B) -> Self
    where
        B: Into<Vec<(K, V)>>,
    {
        let mut res = inp.into();
        res.sort_by(|(left, _), (right, _)| {
            let left = left.borrow();
            let right = right.borrow();
            left.len()
                .cmp(&right.len())
                .reverse()
                .then_with(|| left.as_bytes().cmp(right.as_bytes()))
        });
        super::remove_ordered_dups(&mut res);
        LinearPrefixMap(res.into())
    }
}

impl<K, V> FromIterator<(K, V)> for LinearPrefixMap<K, V>
where
    K: Borrow<str> + Eq,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Self::from_vec(iter.into_iter().collect::<Vec<_>>())
    }
}

impl<K: Borrow<str>, V> PrefixMap<V> for LinearPrefixMap<K, V> {
    fn get_longest_prefix<P: AsRef<str>>(&self, inp: P) -> Option<(usize, &V)> {
        let inp = inp.as_ref();
        for (tag, val) in self.0.iter() {
            let tag = tag.borrow();
            if inp.starts_with(tag) {
                return Some((tag.len(), val));
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{LinearPrefixMap, PrefixMap};

    #[test]
    fn correct_prefixes() {
        let map = LinearPrefixMap::from_vec([("a", 0), ("abc", 1), ("bc", 2), ("bc", 3)]);
        assert_eq!(map.get_longest_prefix("abcd"), Some((3, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((1, &0)));
        assert_eq!(map.get_longest_prefix("bcd"), Some((2, &3)));
        assert_eq!(map.get_longest_prefix("bd"), None);
        assert_eq!(map.get_longest_prefix("💖"), None);
    }

    #[test]
    fn works_for_perverse() {
        let map = LinearPrefixMap::from_vec([("", 0), (" 3", 1)]);
        assert_eq!(map.get_longest_prefix(" 3 "), Some((2, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((0, &0)));
    }
}
