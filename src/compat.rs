use bit_set::BitSet;
use std::collections::BTreeMap;

// This map must offer an iterator preserving the order of insertion.
pub type SmallIntMap<T> = BTreeMap<usize, T>;

pub type BitvSet = BitSet;
