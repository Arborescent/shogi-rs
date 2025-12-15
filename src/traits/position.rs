//! Position trait definitions.

use std::fmt::Debug;

use crate::core::{Color, MoveError};

use super::piece::{PieceT, PieceTypeT};

/// Trait for a move in a shogi variant.
pub trait MoveT: Copy + Clone + Eq + Debug {
    /// The square type for this variant.
    type Square: Copy + Clone + Eq;

    /// The piece type for this variant.
    type PieceType: PieceTypeT;

    /// Returns the destination square of this move.
    fn to(&self) -> Self::Square;

    /// Returns true if this is a drop move.
    fn is_drop(&self) -> bool;

    /// Returns true if this move includes a promotion.
    fn is_promotion(&self) -> bool;
}

/// Trait for a game position in a shogi variant.
pub trait PositionT: Clone + Debug {
    /// The square type for this variant.
    type Square: Copy + Clone + Eq;

    /// The move type for this variant.
    type Move: MoveT<Square = Self::Square, PieceType = Self::PieceType>;

    /// The piece type for this variant.
    type PieceType: PieceTypeT;

    /// The piece (type + color) for this variant.
    type Piece: PieceT<PieceType = Self::PieceType>;

    /// Creates a new position with the standard starting setup.
    fn new() -> Self;

    /// Sets the position from a SFEN string.
    fn set_sfen(&mut self, sfen: &str) -> Result<(), crate::core::SfenError>;

    /// Returns the SFEN string representation of this position.
    fn to_sfen(&self) -> String;

    /// Returns the current side to move.
    fn side_to_move(&self) -> Color;

    /// Returns the piece at the given square, if any.
    fn piece_at(&self, sq: Self::Square) -> Option<Self::Piece>;

    /// Makes the given move on this position.
    fn make_move(&mut self, m: Self::Move) -> Result<(), MoveError>;

    /// Undoes the last move made on this position.
    fn unmake_move(&mut self) -> Result<(), MoveError>;

    /// Returns true if the given color's king is in check.
    fn in_check(&self, c: Color) -> bool;

    /// Returns all legal moves for the side to move.
    fn legal_moves(&self) -> Vec<Self::Move>;

    /// Returns true if the game is over.
    fn is_game_over(&self) -> bool;

    /// Returns the winner, if the game is over.
    fn winner(&self) -> Option<Color>;
}
