use bitcode::{Decode, Encode};
use tokio::io::AsyncReadExt;
use util::{error::TermgameResult, pos::Pos};

use crate::util::AsyncWriteT;

#[derive(Clone, Debug, Encode, Decode)]
pub enum ClientMessage {
  NewConnection,
  ConnectToExisting { uid: u64 },
  PositionUpdate { uid: u64, pos: Pos },
}

#[derive(Clone, Debug, Encode, Decode)]
pub enum ServerMessage {
  NewConnection { uid: u64 },
  ConnectToExisting { success: bool },
  PlayerPositionUpdate { uid: u64, pos: Pos },
  Ping,
}

pub async fn write_message_to_wire<T>(stream: &mut impl AsyncWriteT, message: T) -> TermgameResult
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
  stream: &mut (impl AsyncReadExt + Unpin),
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
