use std::{
  borrow::Borrow,
  collections::{HashMap, HashSet},
  hash::Hash,
  ops::Add,
};

#[derive(Clone, Default)]
pub struct EmptyMetadata();

impl Add for EmptyMetadata {
  type Output = EmptyMetadata;

  fn add(self, _rhs: Self) -> Self::Output {
    EmptyMetadata()
  }
}

struct UfEntry<K, M> {
  metadata: M,
  parent_key: K,
  subtree_size: usize,
}

pub struct UnionFind<K, M = EmptyMetadata> {
  elements: HashMap<K, UfEntry<K, M>>,
  num_groups: usize,
}

impl<K, M> UnionFind<K, M>
where
  K: Clone + Eq + Hash,
  M: Add<Output = M> + Clone + Default,
{
  pub fn from_keys<I>(keys: I) -> Self
  where
    I: IntoIterator<Item = K>,
  {
    Self::from_initializers(keys.into_iter().map(|key| (key, M::default())))
  }
}

impl<K, M> UnionFind<K, M>
where
  K: Clone + Eq + Hash,
  M: Add<Output = M> + Clone,
{
  pub fn from_initializers<I>(initializers: I) -> Self
  where
    I: IntoIterator<Item = (K, M)>,
  {
    let elements: HashMap<_, _> = initializers
      .into_iter()
      .map(|(key, metadata)| {
        (
          key.clone(),
          UfEntry::<K, M> {
            metadata,
            parent_key: key,
            subtree_size: 1,
          },
        )
      })
      .collect();
    let num_groups = elements.len();
    Self { elements, num_groups }
  }

  pub fn keys(&self) -> impl Iterator<Item = &K> {
    self.elements.keys()
  }

  pub fn root_level_keys(&self) -> HashSet<K> {
    self
      .keys()
      .map(|key| self.get_root(key.clone()))
      .collect::<HashSet<_>>()
  }

  /// Unions two keys, returning the new parent of the keys.
  pub fn union(&mut self, k1: K, k2: K) -> K {
    let p1 = self.get_root_mut(k1);
    let p2 = self.get_root_mut(k2);

    if p1 != p2 {
      let p2_entry = self.entry_mut(&p2);
      p2_entry.parent_key = p1.clone();
      let p2_subtree_size = p2_entry.subtree_size;
      let p2_metadata = p2_entry.metadata.clone();

      let p1_entry = self.entry_mut(&p1);
      p1_entry.subtree_size += p2_subtree_size;
      p1_entry.metadata = p1_entry.metadata.clone() + p2_metadata;
      self.num_groups -= 1;
    }

    p1
  }

  /// Returns the parent key for this key.
  pub fn find(&mut self, key: K) -> K {
    self.get_root_mut(key)
  }

  /// Returns the parent key for this key.
  pub fn find_immut(&self, key: K) -> K {
    self.get_root(key)
  }

  pub fn metadata<T>(&self, key: T) -> &M
  where
    T: Borrow<K>,
  {
    &self.entry(key.borrow()).metadata
  }

  pub fn metadata_mut<T>(&mut self, key: T) -> &mut M
  where
    T: Borrow<K>,
  {
    &mut self.entry_mut(key.borrow()).metadata
  }

  fn entry(&self, key: &K) -> &UfEntry<K, M> {
    self.elements.get(key).unwrap()
  }

  fn entry_mut(&mut self, key: &K) -> &mut UfEntry<K, M> {
    self.elements.get_mut(key).unwrap()
  }

  fn parent_key(&self, key: &K) -> K {
    self.entry(key).parent_key.clone()
  }

  fn set_parent_key(&mut self, key: K, new_parent: K) {
    self.entry_mut(&key).parent_key = new_parent;
  }

  fn get_root(&self, key: K) -> K {
    let mut parent = self.parent_key(&key);
    let mut gp = self.parent_key(&parent);
    while parent != gp {
      let ggp = self.parent_key(&gp);
      parent = gp;
      gp = ggp;
    }

    parent
  }

  fn get_root_mut(&mut self, key: K) -> K {
    let mut parent = self.parent_key(&key);

    let mut gp = self.parent_key(&parent);
    while parent != gp {
      let ggp = self.parent_key(&gp);
      self.set_parent_key(parent, ggp.clone());

      parent = gp;
      gp = ggp;
    }

    parent
  }
}

#[cfg(test)]
mod tests {
  use std::iter::empty;

  use super::UnionFind;

  #[test]
  fn test_empty() {
    let uf = UnionFind::<u32>::from_keys(empty());
    assert_eq!(uf.num_groups, 0);
  }

  #[test]
  fn test_empty_with_metadata() {
    let uf = UnionFind::<u32, u64>::from_initializers(empty());
    assert_eq!(uf.num_groups, 0);
  }

  #[test]
  fn test_find() {
    let mut uf = UnionFind::<u32>::from_keys(0..2);
    assert_eq!(uf.num_groups, 2);
    assert_eq!(uf.find(0), 0);
    assert_eq!(uf.find(1), 1);
  }

  #[test]
  fn test_union() {
    let mut uf = UnionFind::<u32>::from_keys(0..4);

    uf.union(0, 1);
    uf.union(2, 3);
    assert_eq!(uf.num_groups, 2);

    uf.union(1, 0);
    assert_eq!(uf.num_groups, 2);
  }

  #[test]
  fn test_metadata() {
    let mut uf =
      UnionFind::<u32, u64>::from_initializers((0..2).map(|idx| (idx, (idx + 1) as u64)));
    assert_eq!(uf.num_groups, 2);
    assert_eq!(*uf.metadata(0), 1);
    assert_eq!(*uf.metadata(1), 2);

    uf.union(0, 1);
    let parent = uf.find(0);
    assert_eq!(*uf.metadata(parent), 3);
    assert_eq!(*uf.metadata(parent), 3);
  }
}
