//! Game rules trait definitions.

use crate::core::Color;

use super::position::PositionT;

/// Result of a game.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameResult {
    /// The game is still in progress.
    InProgress,
    /// The specified color wins.
    Win(Color),
    /// The game is a draw.
    Draw,
}

/// Trait defining the rules for a shogi variant.
pub trait GameRules {
    /// The position type for this variant.
    type Position: PositionT;

    /// Returns the board width for this variant.
    fn board_width() -> u8;

    /// Returns the board height for this variant.
    fn board_height() -> u8;

    /// Returns true if this variant allows drop moves.
    fn allows_drops() -> bool;

    /// Returns the promotion zone depth (number of ranks from the far end).
    fn promotion_zone_depth() -> u8;

    /// Returns true if the given piece type must promote when reaching the back ranks.
    fn must_promote(
        piece_type: <<Self::Position as PositionT>::Piece as super::piece::PieceT>::PieceType,
        to_rank: u8,
        color: Color,
    ) -> bool;

    /// Checks for variant-specific win conditions beyond checkmate.
    ///
    /// Some shogi variants have special win conditions like the "try" rule
    /// where moving the King to the opponent's back rank wins.
    fn check_special_win_conditions(pos: &Self::Position) -> Option<GameResult>;
}
