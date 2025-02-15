use std::collections::HashMap;

use common::msg::{write_message_to_wire, ClientMessage, ServerMessage};
use tokio::net::TcpStream;
use util::error::TermgameResult;

use crate::client_context::ClientContext;

pub struct ServerState {
  clients: HashMap<u64, ClientContext>,
}

impl ServerState {
  pub fn new() -> Self {
    Self { clients: HashMap::new() }
  }

  pub async fn respond_to_message(
    &mut self,
    stream: &mut TcpStream,
    message: ClientMessage,
  ) -> TermgameResult {
    println!("Message: {message:?}");
    write_message_to_wire(stream, ServerMessage::NewConnection { uid: 0 }).await?;
    Ok(())
  }
}
