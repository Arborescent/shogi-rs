//! Trait definitions for shogi variants.
//!
//! These traits define the common interfaces that all shogi variants must implement.
//! This enables writing generic code that works with any variant.

pub mod piece;
pub mod position;
pub mod rules;

pub use piece::PieceTypeT;
pub use position::PositionT;
pub use rules::GameRules;
