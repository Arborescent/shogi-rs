//! Core types shared across all shogi variants.
//!
//! This module contains fundamental types that are generic over board dimensions
//! and don't depend on variant-specific piece types.

pub mod color;
pub mod error;
pub mod square;
pub mod bitboard;
pub mod time;

pub use color::Color;
pub use error::{MoveError, SfenError};
pub use square::{Square, SquareIter};
pub use time::TimeControl;

// Re-export bitboard types
pub use bitboard::Bitboard;

// Type aliases for common board sizes
pub use square::{StandardSquare, MiniSquare};
pub use bitboard::{StandardBitboard, MiniBitboard};
