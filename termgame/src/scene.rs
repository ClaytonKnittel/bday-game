use std::{collections::HashMap, fmt::Display, iter::once};

use rand::{rngs::StdRng, RngCore, SeedableRng};
use termion::event::Key;

use crate::{
  draw::Draw,
  entity::Entity,
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Uid(u64);

impl Display for Uid {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

pub struct Scene {
  entities: HashMap<Uid, Box<dyn Entity>>,
  rng: StdRng,
}

impl Scene {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_entity(&mut self, entity: Box<dyn Entity>) -> Uid {
    let uid = self.next_uid();
    self.entities.insert(uid, entity);
    uid
  }

  pub fn entity<E: Entity>(&self, uid: Uid) -> TermgameResult<&E> {
    self
      .entities
      .get(&uid)
      .and_then(|entity| entity.as_any().downcast_ref::<E>())
      .ok_or_else(|| TermgameError::Internal(format!("Uid not found: {uid}")).into())
  }

  pub fn entity_mut<E: Entity>(&mut self, uid: Uid) -> TermgameResult<&mut E> {
    self
      .entities
      .get_mut(&uid)
      .and_then(|entity| entity.as_any_mut().downcast_mut::<E>())
      .ok_or_else(|| TermgameError::Internal(format!("Uid not found: {uid}")).into())
  }

  pub fn delete_entity(&mut self, uid: Uid) -> TermgameResult {
    self
      .entities
      .remove(&uid)
      .map(|_| ())
      .ok_or_else(|| TermgameError::Internal(format!("Uid not found: {uid}")).into())
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

  fn visit_mut<F, T>(&mut self, mut f: F, arg: T) -> TermgameResult
  where
    F: FnMut(&mut dyn Entity, T) -> TermgameResult,
    T: Clone,
  {
    self
      .entities
      .values_mut()
      .try_for_each(|entity| f(entity.as_mut(), arg.clone()))
  }
}

impl Entity for Scene {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    Box::new(
      self
        .entities
        .values()
        .flat_map(|entity| entity.iterate_tiles()),
    )
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn tick(&mut self, t: usize) -> TermgameResult {
    self.visit_mut(Entity::tick, t)
  }

  fn keypress(&mut self, key: Key) -> TermgameResult {
    self.visit_mut(Entity::keypress, key)
  }

  fn click(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::click, pos)
  }

  fn drag(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::drag, pos)
  }

  fn release(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::release, pos)
  }
}

impl Default for Scene {
  fn default() -> Self {
    Self {
      entities: HashMap::new(),
      rng: StdRng::seed_from_u64(27418995609531717u64),
    }
  }
}
