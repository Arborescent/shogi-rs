//! Wild Cat Shogi (3×5 Shogi) implementation.
//!
//! Wild Cat Shogi is a small variant played on a 3×5 board with modified
//! piece movements. Rook moves like a Wazir (one square orthogonally),
//! Bishop moves like a Fers (one square diagonally).
//!
//! # Pieces
//!
//! Each player has 4 pieces:
//!
//! | Piece | Movement | SFEN | Promotes To |
//! |-------|----------|------|-------------|
//! | King (王/玉) | 1 step any direction | K | - |
//! | Rook (飛) | 1 step orthogonal (Wazir) | R | - |
//! | Bishop (角) | 1 step diagonal (Fers) | B | - |
//! | Pawn (歩) | 1 step forward | P | Gold (金) |
//!
//! # Rules
//!
//! - Board: 3 files × 5 ranks
//! - Promotion zone: Last rank only (1 rank deep)
//! - Drop rules: Same as standard shogi (nifu, pawn last rank)
//! - Win conditions:
//!   - Checkmate
//!   - Flag victory: King reaching opponent's back rank
//! - 4-fold repetition: Draw
//!
//! # SFEN Format
//!
//! Starting position: `bkr/p1p/3/P1P/RKB w - 1`

pub mod moves;
pub mod position;

// Re-export standard types (same piece types as standard shogi, just a subset)
pub use crate::hand::Hand;
pub use crate::piece::Piece;
pub use crate::piece_type::PieceType;
pub use moves::{Move, MoveRecord};
pub use position::Position;

/// Board width for Wild Cat Shogi.
pub const BOARD_WIDTH: u8 = 3;

/// Board height for Wild Cat Shogi.
pub const BOARD_HEIGHT: u8 = 5;

/// Number of squares on the board.
pub const NUM_SQUARES: usize = (BOARD_WIDTH as usize) * (BOARD_HEIGHT as usize);

/// Promotion zone depth (1 rank - the opponent's back rank only).
pub const PROMOTION_ZONE_DEPTH: u8 = 1;

/// Type alias for Wild Cat Shogi square.
pub type Square = crate::core::Square<BOARD_WIDTH, BOARD_HEIGHT>;

/// Type alias for Wild Cat Shogi bitboard.
pub type Bitboard = crate::core::Bitboard<BOARD_WIDTH, BOARD_HEIGHT>;

/// Starting SFEN for Wild Cat Shogi.
/// Layout: Bishop-King-Rook on back ranks, Pawns on second ranks.
pub const STARTING_SFEN: &str = "bkr/p1p/3/P1P/RKB w - 1";

/// Piece types valid in Wild Cat Shogi.
pub const VALID_PIECE_TYPES: [PieceType; 5] = [
    PieceType::King,
    PieceType::Rook,
    PieceType::Bishop,
    PieceType::Pawn,
    PieceType::Gold,
];

/// Hand piece types valid in Wild Cat Shogi.
pub const VALID_HAND_PIECES: [PieceType; 3] = [
    PieceType::Rook,
    PieceType::Bishop,
    PieceType::Pawn,
];

/// Returns true if the piece type is valid in Wild Cat Shogi.
pub fn is_valid_piece_type(pt: PieceType) -> bool {
    matches!(
        pt,
        PieceType::King | PieceType::Rook | PieceType::Bishop | PieceType::Pawn | PieceType::Gold
    )
}

/// Returns true if the piece type can be held in hand in Wild Cat Shogi.
pub fn is_valid_hand_piece(pt: PieceType) -> bool {
    matches!(pt, PieceType::Rook | PieceType::Bishop | PieceType::Pawn)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn board_constants() {
        assert_eq!(BOARD_WIDTH, 3);
        assert_eq!(BOARD_HEIGHT, 5);
        assert_eq!(NUM_SQUARES, 15);
        assert_eq!(PROMOTION_ZONE_DEPTH, 1);
    }

    #[test]
    fn square_creation() {
        // Valid squares
        let sq = Square::new(0, 0);
        assert!(sq.is_some());

        let sq = Square::new(2, 4);
        assert!(sq.is_some());

        // Invalid squares
        let sq = Square::new(3, 0); // file out of bounds
        assert!(sq.is_none());

        let sq = Square::new(0, 5); // rank out of bounds
        assert!(sq.is_none());
    }

    #[test]
    fn bitboard_basic() {
        let bb = Bitboard::empty();
        assert_eq!(bb.count(), 0);

        let sq = Square::new(1, 2).unwrap();
        let bb = Bitboard::from_square(sq);
        assert_eq!(bb.count(), 1);
    }

    #[test]
    fn valid_piece_types() {
        // Valid pieces
        assert!(is_valid_piece_type(PieceType::King));
        assert!(is_valid_piece_type(PieceType::Rook));
        assert!(is_valid_piece_type(PieceType::Bishop));
        assert!(is_valid_piece_type(PieceType::Pawn));
        assert!(is_valid_piece_type(PieceType::Gold));

        // Invalid pieces (not in Wild Cat Shogi)
        assert!(!is_valid_piece_type(PieceType::Silver));
        assert!(!is_valid_piece_type(PieceType::Knight));
        assert!(!is_valid_piece_type(PieceType::Lance));
        assert!(!is_valid_piece_type(PieceType::ProRook));
        assert!(!is_valid_piece_type(PieceType::ProBishop));
        assert!(!is_valid_piece_type(PieceType::ProSilver));
        assert!(!is_valid_piece_type(PieceType::ProKnight));
        assert!(!is_valid_piece_type(PieceType::ProLance));
        assert!(!is_valid_piece_type(PieceType::ProPawn));
    }

    #[test]
    fn valid_hand_pieces() {
        assert!(is_valid_hand_piece(PieceType::Rook));
        assert!(is_valid_hand_piece(PieceType::Bishop));
        assert!(is_valid_hand_piece(PieceType::Pawn));

        // Not droppable
        assert!(!is_valid_hand_piece(PieceType::King));
        assert!(!is_valid_hand_piece(PieceType::Gold));
    }
}
