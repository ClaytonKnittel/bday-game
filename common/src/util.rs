use tokio::io::AsyncWriteExt;

pub trait AsyncWriteT: AsyncWriteExt + Unpin + Send + Sync {}

impl<T> AsyncWriteT for T where T: AsyncWriteExt + Unpin + Send + Sync {}
