use std::{
  collections::HashMap,
  iter::{empty, once},
  sync::Arc,
};

use common::{
  config::MAX_CLUE_LEN,
  crossword::{Crossword, XWordTile},
  msg::{write_message_to_wire, ClientMessage, ServerMessage},
  util::AsyncWriteT,
};
use itertools::Itertools;
use tokio::{
  fs::{self, File},
  io::AsyncWriteExt,
  sync::Mutex,
};
use util::{
  bitcode,
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike},
  pos::Pos,
  time::time_fn,
  variant::Variant2,
};
use xword_dict::XWordDict;
use xword_gen::xword::{XWord, XWordTraits, XWordWithRequired};

use crate::client_context::{AuthenticatedLiveClient, ClientContext, LiveClient};

pub const XWORD_PATH: &str = "./crossword.bin";
pub const XWORD_SCRATCH_PATH: &str = "./crossword_scratch.bin";
pub const DICT_PATH: &str = "../xword_gen/dict.bin";

pub async fn read_dict() -> TermgameResult<XWordDict> {
  Ok(bitcode::decode(&fs::read(DICT_PATH).await?)?)
}

const fn sunday() -> &'static str {
  "________X_______X______
   ________X_______X______
   ________X_______X______
   ___X______X____X___X___
   ____XX_____X______X____
   ______X_____XX____X____
   XXX____X____X____X_____
   ___X_____X______X______
   ________X_____X_____XXX
   _____X_______XXX_______
   _____XX_____X______X___
   ____X_____________X____
   ___X______X_____XX_____
   _______XXX_______X_____
   XXX_____X_____X________
   ______X______X_____X___
   _____X____X____X____XXX
   ____X____XX_____X______
   ____X______X_____XX____
   ___X___X____X______X___
   ______X_______X________
   ______X_______X________
   ______X_______X________"
}

fn make_easy() -> TermgameResult<Grid<XWordTile>> {
  XWord::build_grid(sunday())
}

async fn mega() -> TermgameResult<Grid<XWordTile>> {
  Ok(bitcode::decode(&fs::read("../grid.bin").await?)?)
}

async fn save_crossword(crossword: &Crossword) -> TermgameResult {
  let result = bitcode::encode(crossword.grid());
  let mut file = File::create(XWORD_PATH).await?;
  file.write_all(&result).await?;
  Ok(())
}

enum Action {
  Respond(ServerMessage),
  Broadcast(ServerMessage),
}

#[allow(clippy::large_enum_variant)]
enum State {
  Prompt {
    clue_map: HashMap<u64, (String, String)>,
    solving: bool,
  },
  Crossword {
    crossword_answers: Crossword,
    scratch: Crossword,
    done: bool,
  },
}

pub struct ServerState<W> {
  state: State,
  clients: HashMap<u64, ClientContext<W>>,
  next_uid: u64,
}

impl<W> ServerState<W>
where
  W: AsyncWriteT,
{
  pub fn new() -> Self {
    Self {
      state: State::Prompt { clue_map: HashMap::new(), solving: false },
      clients: HashMap::new(),
      next_uid: 0,
    }
  }

  pub async fn make_crossword(
    &mut self,
    clue_map: HashMap<u64, (String, String)>,
  ) -> TermgameResult<Option<Crossword>> {
    let mut dict = read_dict().await?;
    let mut words: Vec<_> = dict
      .top_n_words(190_000)
      .into_iter()
      .map(|word| word.to_owned())
      .collect_vec();

    let required_words = clue_map
      .values()
      .map(|(word, _)| word.clone())
      .collect_vec();
    words.extend(required_words.iter().cloned());
    println!("Making crossword with {required_words:?}");

    let xword = XWordWithRequired::from_grid(make_easy()?, required_words, words)?;

    let (time, solution) = time_fn(|| xword.solve());

    let solution = solution?;
    println!("Took {}s", time.as_secs_f32());

    if let Some(solution) = solution {
      for (word, clue) in clue_map.values() {
        dict.set_clue(word.clone(), clue.clone());
      }

      let crossword = Crossword::make_clues(solution, &dict)?;
      Ok(Some(crossword))
    } else {
      println!("No solution!");
      Ok(None)
    }
  }

  pub fn with_crossword(crossword_answers: Crossword, scratch: Crossword) -> Self {
    Self {
      state: State::Crossword { crossword_answers, scratch, done: false },
      clients: HashMap::new(),
      next_uid: 0,
    }
  }

  pub fn scratch(&self) -> Option<&Crossword> {
    if let State::Crossword { scratch, .. } = &self.state {
      Some(scratch)
    } else {
      None
    }
  }

  pub fn scratch_mut(&mut self) -> Option<&mut Crossword> {
    if let State::Crossword { scratch, .. } = &mut self.state {
      Some(scratch)
    } else {
      None
    }
  }

  pub fn crossword_answers(&self) -> Option<&Crossword> {
    if let State::Crossword { crossword_answers, .. } = &self.state {
      Some(crossword_answers)
    } else {
      None
    }
  }

  fn is_finished(&self) -> bool {
    self.crossword_answers().is_some_and(|answers| {
      self.scratch().is_some_and(|scratch| {
        answers.grid().positions().all(|pos| {
          match (answers.grid().get(pos), scratch.grid().get(pos)) {
            (Some(t1), Some(t2)) => t1 == t2,
            (None, None) => true,
            _ => false,
          }
        })
      })
    })
  }

  fn assign_new_uid(&mut self) -> u64 {
    let new_uid = self.next_uid;
    self.next_uid += 1;
    new_uid
  }

  fn all_connections(&self) -> impl Iterator<Item = (&u64, &ClientContext<W>)> {
    self.clients.iter()
  }

  fn all_connections_mut(&mut self) -> impl Iterator<Item = (&u64, &mut ClientContext<W>)> {
    self.clients.iter_mut()
  }

  fn live_connections_mut(&mut self) -> impl Iterator<Item = (u64, &mut LiveClient<W>)> {
    self
      .clients
      .iter_mut()
      .filter_map(|(&id, context)| context.as_live_mut().map(|client| (id, client)))
  }

  async fn execute_actions(
    &mut self,
    stream: Arc<Mutex<W>>,
    actions: impl Iterator<Item = Action>,
  ) -> TermgameResult {
    for action in actions {
      match action {
        Action::Respond(message) => {
          write_message_to_wire(&mut *stream.lock().await, message).await?
        }
        Action::Broadcast(message) => {
          for (_, context) in self.live_connections_mut() {
            context.write_message(message.clone()).await?;
          }
        }
      }
    }

    Ok(())
  }

  async fn to_authenticated_mut(
    &mut self,
    uid: u64,
  ) -> TermgameResult<AuthenticatedLiveClient<'_, W>> {
    if let Some(state) = self.clients.get_mut(&uid) {
      if let Some(live_state) = state.as_live_mut() {
        if let Some(auth_client) = live_state.to_authenticated_mut().await {
          Ok(auth_client)
        } else {
          Err(TermgameError::Internal(format!("Cannot authenticate as client {uid}")).into())
        }
      } else {
        Err(TermgameError::Internal(format!("Client {uid} is not live")).into())
      }
    } else {
      Err(TermgameError::Internal(format!("No such client with uid {uid}")).into())
    }
  }

  async fn to_authenticated_context_mut(
    &mut self,
    uid: u64,
  ) -> TermgameResult<&mut ClientContext<W>> {
    self.to_authenticated_mut(uid).await?;
    self
      .clients
      .get_mut(&uid)
      .ok_or_else(|| TermgameError::Internal(format!("No such client with uid {uid}")).into())
  }

  async fn new_connection(
    &mut self,
    stream: Arc<Mutex<W>>,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let new_uid = self.assign_new_uid();

    let client_ctx = ClientContext::new(stream, new_uid);
    let old_val = self.clients.insert(new_uid, client_ctx);
    debug_assert!(old_val.is_none());

    Ok(once(Action::Respond(ServerMessage::NewConnection {
      uid: new_uid,
    })))
  }

  async fn connect_to_existing(
    &mut self,
    stream: Arc<Mutex<W>>,
    uid: u64,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let success = if let Some(state) = self.clients.get_mut(&uid) {
      state.make_live(stream).ok_or_else(|| {
        TermgameError::Internal(format!("Trying to connect to live connection on {uid}"))
      })?;
      true
    } else {
      false
    };

    Ok(once(Action::Respond(ServerMessage::ConnectToExisting {
      success,
    })))
  }

  async fn make_clue(
    &mut self,
    uid: u64,
    word: String,
    clue: String,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    if word.len() >= 3
      && word.len() <= MAX_CLUE_LEN as usize
      && word.chars().all(|c| c.is_ascii_alphabetic())
    {
      match &mut self.state {
        State::Prompt { clue_map, .. } => {
          clue_map.insert(uid, (word.to_ascii_lowercase(), clue));
        }
        _ => {
          println!("Unexpected make clue from {uid}!");
        }
      }
    } else {
      match &mut self.state {
        State::Prompt { clue_map, .. } => {
          clue_map.remove(&uid);
        }
        _ => {}
      }
    }
    Ok(empty())
  }

  async fn build(&mut self, clue_map: HashMap<u64, (String, String)>) -> TermgameResult {
    // let clue_map:HashMap<_,_> = [(14, ("heuristic", "shortcut or approximation")), (15, ("greenbean", "Long boi of the vegetable kingdom, good when spicy")), (3, ("yuck", "Gross, as from a child")), (16, ("umbrella", "rain shield")), (1, ("network", "Fishing effort?")), (12, ("bart", "screaming metal tunnel dragon")), (19, ("lego", "A birthday or Christmas gift children dream for.")), (0, ("freelunch", "The best part about working at Google (two words)")), (8, ("jacky", "Austen's Favorite Nickname")), (11, ("balboa", "This European explorer was credited for discovering the Pacific Ocean in 1513; he was promptly beheaded 6 years later. Multiple establishments in SF are named him.")), (2, ("moonbeam", "nocturnal silver illumination / sounds like a pokemon move")), (4, ("ogee", "This is a feature that defines many historic windows across San Francisco")), (6, ("seoultaco", "best restaurant in stl!")), (7, ("cremia", "a 12% saturated fat ice cream brand, as opposed to the normal 8% saturated fat, that can be found primarily in Japan")), (10, ("odyssey", "what mario goes on in this 2017 game")), (13, ("albatross", "The definitively largest, biggest, and most beautiful flighted bird")), (5, ("fish", "Big ____ - Vince Staples (\"I was up late night ballin'\")")), (17, ("potato", "clayton's least favorite food")), /*(9, ("remy", "best knittel")),*/ (18, ("galapagos", "island by the equator"))].into_iter().map(|(uid, (w, c))| (uid, (w.to_owned(), c.to_owned()))).collect();
    println!("Clues: {clue_map:?}");
    if let Some(crossword) = self.make_crossword(clue_map).await? {
      save_crossword(&crossword).await?;
      let scratch = crossword.clone_clearing_tiles();
      self.state = State::Crossword {
        crossword_answers: crossword,
        scratch,
        done: false,
      };
    }
    Ok(())
  }

  async fn build_xword(&mut self) -> TermgameResult<impl Iterator<Item = Action>> {
    if let State::Prompt { clue_map, solving } = &mut self.state {
      if !*solving {
        *solving = true;
        let clue_map = clue_map.clone();

        let result = self.build(clue_map).await;
        if let State::Prompt { solving, .. } = &mut self.state {
          *solving = false;
        }
        result?;
      }
    }

    Ok(empty())
  }

  async fn position_update(
    &mut self,
    uid: u64,
    pos: Pos,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let context = self.to_authenticated_context_mut(uid).await?;
    context.player_info_mut().pos = pos;
    Ok(once(Action::Broadcast(
      ServerMessage::PlayerPositionUpdate { uid, pos },
    )))
  }

  async fn tile_update(
    &mut self,
    pos: Pos,
    tile: XWordTile,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    if matches!(tile, XWordTile::Wall) {
      return Err(TermgameError::Internal("Cannot place wall tiles".to_owned()).into());
    }
    if let State::Crossword { done, .. } = self.state {
      if done {
        return Ok(Variant2::Opt1(empty()));
      }
    }

    let xword_tile = self
      .scratch_mut()
      .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
      .tile_mut(pos)?;
    match xword_tile {
      XWordTile::Empty | XWordTile::Letter(_) => {
        *xword_tile = tile.clone();
        Ok(Variant2::Opt2(once(Action::Broadcast(
          ServerMessage::TileUpdate { pos, tile },
        ))))
      }
      XWordTile::Wall => {
        Err(TermgameError::Internal(format!("Cannot modify wall tile at {pos}")).into())
      }
    }
  }

  async fn check_tile(&mut self, pos: Pos) -> TermgameResult<impl Iterator<Item = Action>> {
    Ok(
      if let Ok(tile) = self
        .crossword_answers()
        .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
        .tile(pos)
      {
        let response = ServerMessage::CheckTile { pos, tile: tile.clone() };
        if self.is_finished() {
          if let State::Crossword { done, .. } = &mut self.state {
            println!("Crossword is done!");
            *done = true
          } else {
            println!("Crossword is done, but state is wrong!");
          }
        }
        Variant2::Opt1(
          [
            Action::Respond(response.clone()),
            Action::Broadcast(response),
          ]
          .into_iter(),
        )
      } else {
        Variant2::Opt2(empty())
      },
    )
  }

  async fn cycle_clue(
    &mut self,
    pos: Pos,
    is_row: bool,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    if let Some(clue) = self
      .scratch_mut()
      .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
      .clue_for_pos_mut(pos, is_row)
    {
      let clue_entries = clue
        .clue_entries
        .iter()
        .skip(1)
        .chain(clue.clue_entries.first().into_iter())
        .cloned()
        .collect_vec();
      clue.clue_entries = clue_entries;

      Ok(Variant2::Opt1(self.full_refresh().await?))
    } else {
      Ok(Variant2::Opt2(empty()))
    }
  }

  async fn full_refresh(&self) -> TermgameResult<impl Iterator<Item = Action>> {
    if let Some(scratch) = self.scratch() {
      Ok(Variant2::Opt1(once(Action::Respond(
        ServerMessage::FullRefresh {
          crossword: scratch.clone().into(),
          player_info: self
            .all_connections()
            .filter(|(_, connection)| connection.is_live())
            .map(|(&uid, connection)| (uid, connection.player_info().clone()))
            .collect(),
        },
      ))))
    } else {
      Ok(Variant2::Opt2(once(Action::Respond(ServerMessage::OnClue))))
    }
  }

  pub async fn respond_to_message(
    &mut self,
    stream: Arc<Mutex<W>>,
    message: ClientMessage,
  ) -> TermgameResult {
    println!("Message: {message:?}");

    macro_rules! execute {
      ($handler:expr) => {{
        let actions = $handler;
        self.execute_actions(stream, actions).await
      }};
    }

    match message {
      ClientMessage::NewConnection => {
        execute!(self.new_connection(stream.clone()).await?)
      }
      ClientMessage::ConnectToExisting { uid } => {
        execute!(self.connect_to_existing(stream.clone(), uid).await?)
      }
      ClientMessage::MakeClue { uid, word, clue } => {
        execute!(self.make_clue(uid, word, clue).await?)
      }
      ClientMessage::BuildXWord => {
        execute!(self.build_xword().await?)
      }
      ClientMessage::PositionUpdate { uid, pos } => {
        execute!(self.position_update(uid, pos).await?)
      }
      ClientMessage::TileUpdate { pos, tile } => {
        execute!(self.tile_update(pos, tile).await?)
      }
      ClientMessage::CheckTile { pos } => {
        execute!(self.check_tile(pos).await?)
      }
      ClientMessage::CycleClue { pos, is_row } => {
        execute!(self.cycle_clue(pos, is_row).await?)
      }
      ClientMessage::FullRefresh => {
        execute!(self.full_refresh().await?)
      }
    }
  }

  pub async fn cleanup_dead_clients(&mut self) {
    for (uid, client) in self.all_connections_mut() {
      if let Some(live_client) = client.as_live_mut() {
        if !live_client.tcp_writeable().await {
          println!("Client {uid} is dead now");
          client.make_dead();
        }
      }
    }
  }
}
