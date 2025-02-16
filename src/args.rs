use clap::{Parser, ValueEnum};
use serde::Serialize;

#[derive(ValueEnum, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum RunMode {
  InteractiveGrid,
  Progress,
  Play,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
  #[arg(long, default_value = "play")]
  pub mode: RunMode,

  #[arg(long)]
  pub admin: bool,

  #[arg(long, default_value = "cknittel.com")]
  pub host: String,
}
