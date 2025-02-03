use std::{collections::HashMap, iter::once};

use rand::{rngs::StdRng, RngCore, SeedableRng};

use crate::{draw::Draw, entity::Entity, pos::Pos};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Uid(u64);

pub struct Scene<'a> {
  entities: HashMap<Uid, Box<dyn Entity + 'a>>,
  rng: StdRng,
}

impl<'a> Scene<'a> {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_entity(&mut self, entity: Box<dyn Entity + 'a>) -> Uid {
    let uid = self.next_uid();
    self.entities.insert(uid, entity);
    uid
  }

  fn next_uid(&mut self) -> Uid {
    #[allow(clippy::unwrap_used)]
    once(())
      .cycle()
      .find_map(|_| {
        let uid = Uid(self.rng.next_u64());
        (!self.entities.contains_key(&uid)).then_some(uid)
      })
      .unwrap()
  }

  fn visit_mut<'b, F, T>(&mut self, mut f: F, arg: T)
  where
    'a: 'b,
    F: FnMut(&mut (dyn Entity + 'b), T),
    T: Clone,
  {
    self
      .entities
      .values_mut()
      .for_each(|entity| f(entity.as_mut(), arg.clone()));
  }
}

impl Entity for Scene<'_> {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    Box::new(
      self
        .entities
        .values()
        .flat_map(|entity| entity.iterate_tiles()),
    )
  }

  fn tick(&mut self, t: usize) {
    self.visit_mut(Entity::tick, t);
  }

  fn click(&mut self, pos: Pos) {
    self.visit_mut(Entity::click, pos);
  }

  fn drag(&mut self, pos: Pos) {
    self.visit_mut(Entity::drag, pos);
  }

  fn release(&mut self, pos: Pos) {
    self.visit_mut(Entity::release, pos);
  }
}

impl Default for Scene<'_> {
  fn default() -> Self {
    Self {
      entities: HashMap::new(),
      rng: StdRng::seed_from_u64(27418995609531717u64),
    }
  }
}
