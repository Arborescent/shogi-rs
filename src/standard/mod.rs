//! Standard 9×9 Shogi implementation.
//!
//! This module contains the complete implementation for standard (Honshogi) 9×9 Shogi,
//! including piece types, positions, move generation, and attack tables.
//!
//! For backward compatibility, the main implementation currently remains in the root-level modules.
//! This module re-exports types from the root and will eventually contain the full implementation.

// Note: For now, these submodules are not used - we re-export from root modules
// These exist as placeholders for future complete modularization
// pub mod piece_type;
// pub mod piece;
// pub mod hand;
// pub mod moves;
// pub mod position;
// pub mod factory;

/// Board width for standard Shogi.
pub const BOARD_WIDTH: u8 = 9;

/// Board height for standard Shogi.
pub const BOARD_HEIGHT: u8 = 9;

/// Number of squares on the board.
pub const NUM_SQUARES: usize = (BOARD_WIDTH as usize) * (BOARD_HEIGHT as usize);

/// Promotion zone depth (3 ranks from the far end).
pub const PROMOTION_ZONE_DEPTH: u8 = 3;

// Re-export standard shogi types from root modules for now
pub use crate::piece_type::{PieceType, PieceTypeIter};
pub use crate::piece::Piece;
pub use crate::hand::Hand;
pub use crate::moves::StandardMove as Move;
pub use crate::position::{Position, ImpasseResult, GameStatus};
pub use crate::record::{MoveRecord, NotationFormat};
pub use crate::bitboard::Factory;
pub use crate::square::StandardSquare as Square;
pub use crate::bitboard::StandardBitboard as Bitboard;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Color;

    #[test]
    fn board_constants() {
        assert_eq!(BOARD_WIDTH, 9);
        assert_eq!(BOARD_HEIGHT, 9);
        assert_eq!(NUM_SQUARES, 81);
        assert_eq!(PROMOTION_ZONE_DEPTH, 3);
    }

    #[test]
    fn standard_types_accessible() {
        // Verify types are accessible through the standard module
        let _pt = PieceType::Pawn;
        let _p = Piece { piece_type: PieceType::King, color: Color::Black };
        let _sq = Square::new(4, 4).unwrap();
        let _bb = Bitboard::empty();
        let _hand: Hand = Default::default();
    }

    #[test]
    fn standard_move_creation() {
        let sq1 = Square::new(6, 6).unwrap(); // 7g
        let sq2 = Square::new(6, 5).unwrap(); // 7f

        let mv = Move::Normal { from: sq1, to: sq2, promote: false };
        assert!(!matches!(mv, Move::Drop { .. }));

        let drop = Move::Drop { to: sq2, piece_type: PieceType::Pawn };
        assert!(matches!(drop, Move::Drop { .. }));
    }

    #[test]
    fn standard_position_creation() {
        Factory::init();
        let pos = Position::new();
        assert_eq!(pos.side_to_move(), Color::Black);
    }
}
