use super::PrefixMap;
use fst::{Automaton, IntoStreamer, Map, MapBuilder, Streamer};
#[cfg(feature = "indexmap")]
use indexmap::IndexSet;
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
struct Prefixes<'a>(&'a [u8]);

#[derive(Debug, Clone, Copy)]
enum PrefixesState {
    Pos(usize),
    Fail,
    Success,
}

impl<'a> Automaton for Prefixes<'a> {
    type State = PrefixesState;

    fn start(&self) -> Self::State {
        PrefixesState::Pos(0)
    }

    fn is_match(&self, state: &Self::State) -> bool {
        matches!(state, PrefixesState::Success)
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        match state {
            &PrefixesState::Pos(ind) if self.0.get(ind) == Some(&byte) => {
                PrefixesState::Pos(ind + 1)
            }
            _ => PrefixesState::Fail,
        }
    }

    fn accept_eof(&self, state: &Self::State) -> Option<Self::State> {
        match state {
            PrefixesState::Pos(_) => Some(PrefixesState::Success),
            _ => None,
        }
    }
}

/// A prefix map that uses Finite State Machines to find matching tokens
///
/// This prefix map requires the `"fst"` feature, and is likely the most memory efficient, but is
/// decidely [slower][crate::prefix_map] than other implementations. However, it also has an
/// optimal order time of `O(longest_prefix)`.
///
/// # Example
/// ```
/// use asciimath_parser::prefix_map::FstPrefixMap;
/// use asciimath_parser::{ASCIIMATH_TOKENS};
///
/// let token_map = FstPrefixMap::from_iter(ASCIIMATH_TOKENS);
/// ```
#[derive(Debug, Clone)]
pub struct FstPrefixMap<V> {
    map: Map<Vec<u8>>,
    // TODO is current usage Vs are enums, so we can convert, but the interface returns a reference
    // not a value making this difficult. It also requires using an external lib to efficiently
    // order the keys
    values: Box<[V]>,
}

impl<V> FstPrefixMap<V>
where
    V: Hash + Eq,
{
    /// Create from a vector of entries
    ///
    /// Due to the interface of [PrefixMap] this requires a little extra heap allocation that could
    /// be avoided with a more concise interface for prefixes, but since this is already orders of
    /// magnitude slower than competing maps, this is mostly provided as a proof of concept.
    pub fn from_vec<K, B>(inp: B) -> Self
    where
        K: Borrow<str> + Eq,
        B: Into<Vec<(K, V)>>,
    {
        let mut ordered = inp.into();
        ordered.sort_by(|(left, _), (right, _)| {
            left.borrow().as_bytes().cmp(right.borrow().as_bytes())
        });
        super::remove_ordered_dups(&mut ordered);
        let mut build = MapBuilder::memory();
        let mut inds = HashMap::new();
        for (key, val) in ordered {
            let next = inds.len();
            let val_ind = *match inds.entry(val) {
                Entry::Occupied(ent) => ent.into_mut(),
                Entry::Vacant(ent) => ent.insert(next),
            };
            // unwrap safe in most contexts usize -> u64
            let val = val_ind.try_into().unwrap();
            // unwrap is safe because in memory
            build.insert(key.borrow().as_bytes(), val).unwrap();
        }

        // costly (n log n) sort by index
        let mut ents: Vec<_> = inds.into_iter().collect();
        ents.sort_unstable_by_key(|&(_, k)| k);
        FstPrefixMap {
            map: build.into_map(),
            values: ents.into_iter().map(|(v, _)| v).collect(),
        }
    }
}

impl<K, V> FromIterator<(K, V)> for FstPrefixMap<V>
where
    K: Borrow<str> + Eq,
    V: Hash + Eq,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Self::from_vec(iter.into_iter().collect::<Vec<_>>())
    }
}

impl<V> PrefixMap<V> for FstPrefixMap<V> {
    fn get_longest_prefix<K>(&self, inp: K) -> Option<(usize, &V)>
    where
        K: AsRef<str>,
    {
        // NOTE for some reason, prefixes doesn't work for empty slice
        let mut res = self
            .map
            .get([])
            .map(|ind| (0, &self.values[usize::try_from(ind).unwrap()]));
        let matcher = Prefixes(inp.as_ref().as_bytes());
        let mut stream = self.map.search(&matcher).into_stream();
        while let Some((k, v)) = stream.next() {
            match res {
                Some((len, _)) if k.len() <= len => {}
                _ => {
                    let ind: usize = v.try_into().unwrap();
                    res = Some((k.len(), &self.values[ind]));
                }
            }
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::{FstPrefixMap, PrefixMap};

    #[test]
    fn correct_prefixes() {
        let map = FstPrefixMap::from_vec([("a", 0), ("abc", 1), ("bc", 2), ("bc", 3)]);
        assert_eq!(map.get_longest_prefix("abcd"), Some((3, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((1, &0)));
        assert_eq!(map.get_longest_prefix("bca"), Some((2, &3)));
        assert_eq!(map.get_longest_prefix("bd"), None);
        assert_eq!(map.get_longest_prefix("💖"), None);
    }

    #[test]
    fn works_for_perverse() {
        let map = FstPrefixMap::from_vec([("", 0), (" 3", 1)]);
        assert_eq!(map.get_longest_prefix(" 3 "), Some((2, &1)));
        assert_eq!(map.get_longest_prefix("ab"), Some((0, &0)));
    }
}
