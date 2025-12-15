//! Mini Shogi (5×5 Shogi) implementation.
//!
//! Mini Shogi is a smaller variant of shogi played on a 5×5 board with
//! a reduced set of pieces. It was designed to provide a quicker game
//! while retaining the strategic depth of standard shogi.
//!
//! # Pieces
//!
//! Each player has 6 pieces (no Knight or Lance):
//!
//! | Piece | Movement | SFEN | Promotes To |
//! |-------|----------|------|-------------|
//! | King (王/玉) | 1 step any direction | K | - |
//! | Rook (飛) | Any number orthogonal | R | Dragon (龍) |
//! | Bishop (角) | Any number diagonal | B | Horse (馬) |
//! | Gold (金) | Gold general pattern | G | - |
//! | Silver (銀) | Silver general pattern | S | Promoted Silver |
//! | Pawn (歩) | 1 step forward | P | Tokin (と) |
//!
//! # Rules
//!
//! - Board: 5 files × 5 ranks
//! - Promotion zone: Last rank only (1 rank deep)
//! - Drop rules: Same as standard shogi (nifu, uchifuzume, pawn last rank)
//! - Win condition: Checkmate (no try rule)
//!
//! # SFEN Format
//!
//! Starting position: `rbsgk/4p/5/P4/KGSBR b - 1`

pub mod moves;
pub mod position;

// Re-export standard types (same piece types as standard shogi, just a subset)
pub use crate::piece_type::PieceType;
pub use crate::piece::Piece;
pub use crate::hand::Hand;
pub use moves::{Move, MoveRecord};
pub use position::Position;

/// Board width for Mini Shogi.
pub const BOARD_WIDTH: u8 = 5;

/// Board height for Mini Shogi.
pub const BOARD_HEIGHT: u8 = 5;

/// Number of squares on the board.
pub const NUM_SQUARES: usize = (BOARD_WIDTH as usize) * (BOARD_HEIGHT as usize);

/// Promotion zone depth (1 rank - the opponent's back rank only).
pub const PROMOTION_ZONE_DEPTH: u8 = 1;

/// Type alias for Mini Shogi square.
pub type Square = crate::core::Square<BOARD_WIDTH, BOARD_HEIGHT>;

/// Type alias for Mini Shogi bitboard.
pub type Bitboard = crate::core::Bitboard<BOARD_WIDTH, BOARD_HEIGHT>;

/// Starting SFEN for Mini Shogi.
/// Layout: Rook-Bishop-Silver-Gold-King on back ranks, Pawn in front.
pub const STARTING_SFEN: &str = "rbsgk/4p/5/P4/KGSBR b - 1";

/// Piece types valid in Mini Shogi (no Knight or Lance).
pub const VALID_PIECE_TYPES: [PieceType; 10] = [
    PieceType::King,
    PieceType::Rook,
    PieceType::Bishop,
    PieceType::Gold,
    PieceType::Silver,
    PieceType::Pawn,
    PieceType::ProRook,
    PieceType::ProBishop,
    PieceType::ProSilver,
    PieceType::ProPawn,
];

/// Hand piece types valid in Mini Shogi.
pub const VALID_HAND_PIECES: [PieceType; 5] = [
    PieceType::Rook,
    PieceType::Bishop,
    PieceType::Gold,
    PieceType::Silver,
    PieceType::Pawn,
];

/// Returns true if the piece type is valid in Mini Shogi.
pub fn is_valid_piece_type(pt: PieceType) -> bool {
    !matches!(pt, PieceType::Knight | PieceType::Lance | PieceType::ProKnight | PieceType::ProLance)
}

/// Returns true if the piece type can be held in hand in Mini Shogi.
pub fn is_valid_hand_piece(pt: PieceType) -> bool {
    matches!(
        pt,
        PieceType::Rook | PieceType::Bishop | PieceType::Gold | PieceType::Silver | PieceType::Pawn
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn board_constants() {
        assert_eq!(BOARD_WIDTH, 5);
        assert_eq!(BOARD_HEIGHT, 5);
        assert_eq!(NUM_SQUARES, 25);
        assert_eq!(PROMOTION_ZONE_DEPTH, 1);
    }

    #[test]
    fn square_creation() {
        // Valid squares
        let sq = Square::new(0, 0);
        assert!(sq.is_some());

        let sq = Square::new(4, 4);
        assert!(sq.is_some());

        // Invalid squares
        let sq = Square::new(5, 0); // file out of bounds
        assert!(sq.is_none());

        let sq = Square::new(0, 5); // rank out of bounds
        assert!(sq.is_none());
    }

    #[test]
    fn bitboard_basic() {
        let bb = Bitboard::empty();
        assert_eq!(bb.count(), 0);

        let sq = Square::new(2, 2).unwrap();
        let bb = Bitboard::from_square(sq);
        assert_eq!(bb.count(), 1);
    }

    #[test]
    fn valid_piece_types() {
        // Valid pieces
        assert!(is_valid_piece_type(PieceType::King));
        assert!(is_valid_piece_type(PieceType::Rook));
        assert!(is_valid_piece_type(PieceType::Bishop));
        assert!(is_valid_piece_type(PieceType::Gold));
        assert!(is_valid_piece_type(PieceType::Silver));
        assert!(is_valid_piece_type(PieceType::Pawn));
        assert!(is_valid_piece_type(PieceType::ProRook));
        assert!(is_valid_piece_type(PieceType::ProBishop));
        assert!(is_valid_piece_type(PieceType::ProSilver));
        assert!(is_valid_piece_type(PieceType::ProPawn));

        // Invalid pieces (not in Mini Shogi)
        assert!(!is_valid_piece_type(PieceType::Knight));
        assert!(!is_valid_piece_type(PieceType::Lance));
        assert!(!is_valid_piece_type(PieceType::ProKnight));
        assert!(!is_valid_piece_type(PieceType::ProLance));
    }

    #[test]
    fn valid_hand_pieces() {
        assert!(is_valid_hand_piece(PieceType::Rook));
        assert!(is_valid_hand_piece(PieceType::Bishop));
        assert!(is_valid_hand_piece(PieceType::Gold));
        assert!(is_valid_hand_piece(PieceType::Silver));
        assert!(is_valid_hand_piece(PieceType::Pawn));

        // Not droppable
        assert!(!is_valid_hand_piece(PieceType::King));
        assert!(!is_valid_hand_piece(PieceType::ProRook));
    }
}
