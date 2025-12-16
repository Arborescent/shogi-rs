//! A library for implementing Shogi application.
//!
//! `shogi` provides a various types and implementations for representing concepts and rules in Shogi.
//! Most types can be created programatically while they can also be deserialized from / serialized to SFEN format.
//! See [USIプロトコルとは (What is the USI protocol?)](http://shogidokoro.starfree.jp/usi.html) for more detail about UCI protocol specification and SFEN format.
//!
//! # Module Organization
//!
//! The library is organized into several module groups:
//!
//! - **core**: Generic types shared across all shogi variants (Color, Square, Bitboard, Error)
//! - **traits**: Trait definitions for variant-agnostic code (PieceTypeT, PositionT, GameRules)
//! - **standard**: Standard 9×9 Shogi implementation (re-exports from root for now)
//!
//! # Board Size Support
//!
//! This library supports multiple board sizes through const generics:
//! - Standard Shogi (9×9): Use the default `Square`, `Bitboard`, `Move`, `Position` types
//! - Mini Shogi (5×5): Use the `minishogi` module for complete variant support
//!
//! # Examples
//!
//! ```
//! use shogi::{Move, Position};
//! use shogi::bitboard::Factory as BBFactory;
//! use shogi::square::consts::*;
//!
//! BBFactory::init();
//! let mut pos = Position::new();
//!
//! // Position can be set from the SFEN formatted string.
//! pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();
//!
//! // You can programatically create a Move instance.
//! let m = Move::Normal{from: SQ_7G, to: SQ_7F, promote: false};
//! pos.make_move(m).unwrap();
//!
//! // Move can be created from the SFEN formatted string as well.
//! let m = Move::from_sfen("7c7d").unwrap();
//! pos.make_move(m).unwrap();
//!
//! // Position can be converted back to the SFEN formatted string.
//! assert_eq!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f 7c7d", pos.to_sfen());
//! ```
#![recursion_limit = "81"]

// ============================================================================
// New modular structure (core types and traits)
// ============================================================================

pub mod core;
pub mod traits;
pub mod standard;
pub mod minishogi;
pub mod wildcatshogi;

// ============================================================================
// Original modules (retained for backward compatibility)
// ============================================================================

pub mod bitboard;
pub mod color;
pub mod error;
pub mod hand;
pub mod moves;
pub mod piece;
pub mod piece_type;
pub mod position;
pub mod record;
pub mod sfen;
pub mod square;
pub mod time;

// ============================================================================
// Re-exports for backward compatibility
// ============================================================================

// Re-export standard 9×9 types as defaults for backward compatibility
pub use self::bitboard::StandardBitboard as Bitboard;
pub use self::color::Color;
pub use self::error::{MoveError, SfenError};
pub use self::hand::Hand;
pub use self::moves::{ParseMoveError, StandardMove as Move};
pub use self::piece::Piece;
pub use self::piece_type::PieceType;
pub use self::position::{GameStatus, ImpasseResult, Position};
pub use self::record::{MoveRecord, NotationFormat};
pub use self::sfen::Sfen;
pub use self::square::{ParseSquareError, StandardSquare as Square};
pub use self::time::TimeControl;
