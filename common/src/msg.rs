use bitcode::{Decode, Encode};
use tokio::{
  io::{AsyncReadExt, AsyncWriteExt},
  net::TcpStream,
};
use util::error::TermgameResult;

#[derive(Debug, Encode, Decode)]
pub enum ClientMessage {
  TestMessage(String),
}

#[derive(Debug, Encode, Decode)]
pub enum ServerMessage {
  TestServerMessage(String),
}

pub async fn write_message_to_wire<T>(stream: &mut TcpStream, message: T) -> TermgameResult
where
  T: Encode,
{
  let encoded = bitcode::encode(&message);
  let len = encoded.len();

  stream.write_u64(len as u64).await?;
  stream.write_all(&encoded).await?;
  Ok(())
}

pub enum DecodeMessageResult<T> {
  Message(T),
  EndOfStream,
}

pub async fn read_message_from_wire<T>(
  stream: &mut TcpStream,
) -> TermgameResult<DecodeMessageResult<T>>
where
  T: for<'a> Decode<'a>,
{
  // TODO check for end of stream.
  let len = stream.read_u64().await?;

  let mut buf = vec![0u8; len as usize];
  stream.read_exact(buf.as_mut_slice()).await?;
  if buf.is_empty() {
    Ok(DecodeMessageResult::EndOfStream)
  } else {
    Ok(DecodeMessageResult::Message(bitcode::decode(
      buf.as_slice(),
    )?))
  }
}
