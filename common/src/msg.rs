use bitcode::{Decode, Encode};

#[derive(Debug, Encode, Decode)]
pub enum ClientMessage {
  TestMessage(String),
}
