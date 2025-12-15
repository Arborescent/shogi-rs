//! Piece type trait definitions.

use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Trait for piece types in shogi variants.
///
/// Each variant (standard, mini, etc.) defines its own piece type enum
/// that implements this trait.
pub trait PieceTypeT: Copy + Clone + Eq + Hash + Debug + Display + Sized {
    /// Returns true if this piece type can promote.
    fn can_promote(&self) -> bool;

    /// Returns the promoted version of this piece type, if it can promote.
    fn promote(&self) -> Option<Self>;

    /// Returns the unpromoted version of this piece type, if it is promoted.
    fn unpromote(&self) -> Option<Self>;

    /// Returns true if this piece type can be held in hand (for drop moves).
    fn is_hand_piece(&self) -> bool;

    /// Returns the SFEN character representation of this piece type.
    fn to_sfen_char(&self) -> char;

    /// Creates a piece type from its SFEN character representation.
    fn from_sfen_char(c: char) -> Option<Self>;

    /// Returns an iterator over all piece types in this variant.
    fn iter() -> impl Iterator<Item = Self>;

    /// Returns the unique index for this piece type (for array indexing).
    fn index(&self) -> usize;

    /// Returns the total number of piece types in this variant.
    fn count() -> usize;
}

/// Trait for a piece (piece type + color).
pub trait PieceT: Copy + Clone + Eq + Hash + Debug {
    /// The piece type for this variant.
    type PieceType: PieceTypeT;

    /// Returns the piece type.
    fn piece_type(&self) -> Self::PieceType;

    /// Returns the color of this piece.
    fn color(&self) -> crate::Color;

    /// Returns a new piece with the opposite color.
    fn flip(&self) -> Self;

    /// Returns the promoted version of this piece, if it can promote.
    fn promote(&self) -> Option<Self>;

    /// Returns the unpromoted version of this piece, if it is promoted.
    fn unpromote(&self) -> Option<Self>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Color;
    use crate::piece::Piece;
    use crate::piece_type::PieceType;

    // Test that PieceType implements PieceTypeT correctly
    #[test]
    fn piece_type_trait_can_promote() {
        // Test via trait method
        assert!(<PieceType as PieceTypeT>::can_promote(&PieceType::Pawn));
        assert!(<PieceType as PieceTypeT>::can_promote(&PieceType::Rook));
        assert!(!<PieceType as PieceTypeT>::can_promote(&PieceType::Gold));
        assert!(!<PieceType as PieceTypeT>::can_promote(&PieceType::King));
        assert!(!<PieceType as PieceTypeT>::can_promote(&PieceType::ProPawn));
    }

    #[test]
    fn piece_type_trait_promote_unpromote() {
        // Test promote via trait
        assert_eq!(
            <PieceType as PieceTypeT>::promote(&PieceType::Pawn),
            Some(PieceType::ProPawn)
        );
        assert_eq!(
            <PieceType as PieceTypeT>::promote(&PieceType::Gold),
            None
        );

        // Test unpromote via trait
        assert_eq!(
            <PieceType as PieceTypeT>::unpromote(&PieceType::ProPawn),
            Some(PieceType::Pawn)
        );
        assert_eq!(
            <PieceType as PieceTypeT>::unpromote(&PieceType::Pawn),
            None
        );
    }

    #[test]
    fn piece_type_trait_is_hand_piece() {
        assert!(<PieceType as PieceTypeT>::is_hand_piece(&PieceType::Pawn));
        assert!(<PieceType as PieceTypeT>::is_hand_piece(&PieceType::Rook));
        assert!(!<PieceType as PieceTypeT>::is_hand_piece(&PieceType::King));
        assert!(!<PieceType as PieceTypeT>::is_hand_piece(&PieceType::ProPawn));
    }

    #[test]
    fn piece_type_trait_sfen_roundtrip() {
        for pt in <PieceType as PieceTypeT>::iter() {
            let c = <PieceType as PieceTypeT>::to_sfen_char(&pt);
            // Only unpromoted pieces can be parsed from SFEN char
            if pt.unpromote().is_none() && pt != PieceType::King {
                let parsed = <PieceType as PieceTypeT>::from_sfen_char(c);
                assert_eq!(parsed, Some(pt), "Failed for {:?}", pt);
            }
        }
    }

    #[test]
    fn piece_type_trait_iter_count() {
        let count = <PieceType as PieceTypeT>::iter().count();
        assert_eq!(count, <PieceType as PieceTypeT>::count());
        assert_eq!(count, 14); // Standard shogi has 14 piece types
    }

    #[test]
    fn piece_type_trait_index_unique() {
        let mut indices = std::collections::HashSet::new();
        for pt in <PieceType as PieceTypeT>::iter() {
            let idx = <PieceType as PieceTypeT>::index(&pt);
            assert!(indices.insert(idx), "Duplicate index {} for {:?}", idx, pt);
        }
        assert_eq!(indices.len(), 14);
    }

    // Test that Piece implements PieceT correctly
    #[test]
    fn piece_trait_piece_type() {
        let p = Piece {
            piece_type: PieceType::Pawn,
            color: Color::Black,
        };
        assert_eq!(<Piece as PieceT>::piece_type(&p), PieceType::Pawn);
    }

    #[test]
    fn piece_trait_color() {
        let black_pawn = Piece {
            piece_type: PieceType::Pawn,
            color: Color::Black,
        };
        let white_pawn = Piece {
            piece_type: PieceType::Pawn,
            color: Color::White,
        };
        assert_eq!(<Piece as PieceT>::color(&black_pawn), Color::Black);
        assert_eq!(<Piece as PieceT>::color(&white_pawn), Color::White);
    }

    #[test]
    fn piece_trait_flip() {
        let black_pawn = Piece {
            piece_type: PieceType::Pawn,
            color: Color::Black,
        };
        let flipped = <Piece as PieceT>::flip(&black_pawn);
        assert_eq!(<Piece as PieceT>::color(&flipped), Color::White);
        assert_eq!(<Piece as PieceT>::piece_type(&flipped), PieceType::Pawn);
    }

    #[test]
    fn piece_trait_promote_unpromote() {
        let pawn = Piece {
            piece_type: PieceType::Pawn,
            color: Color::Black,
        };
        let promoted = <Piece as PieceT>::promote(&pawn).unwrap();
        assert_eq!(<Piece as PieceT>::piece_type(&promoted), PieceType::ProPawn);
        assert_eq!(<Piece as PieceT>::color(&promoted), Color::Black);

        let unpromoted = <Piece as PieceT>::unpromote(&promoted).unwrap();
        assert_eq!(unpromoted, pawn);
    }
}
