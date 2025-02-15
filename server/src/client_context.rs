use tokio::net::TcpStream;

enum ClientState {}

pub struct ClientContext {
  stream: TcpStream,
}

impl ClientContext {
  pub fn new(stream: TcpStream) -> Self {
    Self { stream }
  }
}
