//! Move representation for Mini Shogi.

use std::fmt;

use crate::Color;

use super::{is_valid_hand_piece, Piece, PieceType, Square};

/// Represents a move in Mini Shogi.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Move {
    /// A normal move from one square to another.
    Normal {
        from: Square,
        to: Square,
        /// Promotion happens when entering the last rank.
        promote: bool,
    },
    /// A drop move placing a piece from hand onto the board.
    Drop {
        to: Square,
        piece_type: PieceType,
    },
}

impl Move {
    /// Creates a Move from SFEN formatted string.
    ///
    /// Format: "1a2a" for normal moves, "1a2a+" for promotion, "P*2b" for drops.
    pub fn from_sfen(s: &str) -> Option<Self> {
        if s.len() < 4 {
            return None;
        }

        let chars: Vec<char> = s.chars().collect();

        // Check for drop move (e.g., "P*2b")
        if chars.len() >= 4 && chars[1] == '*' {
            let piece_type = PieceType::from_sfen(chars[0])?;
            if !is_valid_hand_piece(piece_type) {
                return None;
            }
            let to = Square::from_sfen(&s[2..4])?;
            return Some(Move::Drop { to, piece_type });
        }

        // Normal move
        if s.len() != 4 && (s.len() != 5 || chars[4] != '+') {
            return None;
        }

        let from = Square::from_sfen(&s[0..2])?;
        let to = Square::from_sfen(&s[2..4])?;
        let promote = s.len() == 5;

        Some(Move::Normal { from, to, promote })
    }

    /// Returns the destination square of this move.
    pub fn to(&self) -> Square {
        match *self {
            Move::Normal { to, .. } => to,
            Move::Drop { to, .. } => to,
        }
    }

    /// Returns true if this is a drop move.
    pub fn is_drop(&self) -> bool {
        matches!(self, Move::Drop { .. })
    }

    /// Returns true if this move includes promotion.
    pub fn is_promotion(&self) -> bool {
        matches!(self, Move::Normal { promote: true, .. })
    }

    /// Formats the move in Japanese notation.
    pub fn to_japanese(&self, piece_type: PieceType, color: Color) -> String {
        let symbol = match color {
            Color::Black => "☗",
            Color::White => "☖",
        };

        match *self {
            Move::Normal { to, promote, .. } => {
                let file_str = file_to_japanese(to.file());
                let rank_str = rank_to_kanji(to.rank());
                let piece_str = piece_type_to_kanji(piece_type);

                if promote {
                    format!("{}{}{}{}成", symbol, file_str, rank_str, piece_str)
                } else {
                    format!("{}{}{}{}", symbol, file_str, rank_str, piece_str)
                }
            }
            Move::Drop { to, piece_type: pt } => {
                let file_str = file_to_japanese(to.file());
                let rank_str = rank_to_kanji(to.rank());
                let piece_str = piece_type_to_kanji(pt);

                format!("{}{}{}{}打", symbol, file_str, rank_str, piece_str)
            }
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Move::Normal { from, to, promote } => {
                write!(f, "{}{}{}", from, to, if promote { "+" } else { "" })
            }
            Move::Drop { to, piece_type } => {
                write!(f, "{}*{}", piece_type_to_sfen_char(piece_type), to)
            }
        }
    }
}

/// MoveRecord stores information necessary to undo a move.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MoveRecord {
    Normal {
        from: Square,
        to: Square,
        placed: Piece,
        captured: Option<Piece>,
        promoted: bool,
    },
    Drop {
        to: Square,
        piece: Piece,
    },
}

impl MoveRecord {
    /// Converts the move record to SFEN format.
    pub fn to_sfen(&self) -> String {
        match *self {
            MoveRecord::Normal {
                from, to, promoted, ..
            } => format!("{}{}{}", from, to, if promoted { "+" } else { "" }),
            MoveRecord::Drop {
                to,
                piece: Piece { piece_type, .. },
            } => format!("{}*{}", piece_type_to_sfen_char(piece_type), to),
        }
    }

    /// Formats the move in Japanese notation.
    pub fn to_japanese(&self) -> String {
        match *self {
            MoveRecord::Normal {
                to,
                ref placed,
                promoted,
                ..
            } => {
                let symbol = match placed.color {
                    Color::Black => "☗",
                    Color::White => "☖",
                };

                let piece_type = if promoted {
                    placed.piece_type.unpromote().unwrap_or(placed.piece_type)
                } else {
                    placed.piece_type
                };

                let file_str = file_to_japanese(to.file());
                let rank_str = rank_to_kanji(to.rank());
                let piece_str = piece_type_to_kanji(piece_type);

                if promoted {
                    format!("{}{}{}{}成", symbol, file_str, rank_str, piece_str)
                } else {
                    format!("{}{}{}{}", symbol, file_str, rank_str, piece_str)
                }
            }
            MoveRecord::Drop { to, ref piece } => {
                let symbol = match piece.color {
                    Color::Black => "☗",
                    Color::White => "☖",
                };

                let file_str = file_to_japanese(to.file());
                let rank_str = rank_to_kanji(to.rank());
                let piece_str = piece_type_to_kanji(piece.piece_type);

                format!("{}{}{}{}打", symbol, file_str, rank_str, piece_str)
            }
        }
    }
}

impl PartialEq<Move> for MoveRecord {
    fn eq(&self, other: &Move) -> bool {
        match (self, other) {
            (
                MoveRecord::Normal {
                    from: f1,
                    to: t1,
                    promoted,
                    ..
                },
                Move::Normal {
                    from: f2,
                    to: t2,
                    promote,
                },
            ) => f1 == f2 && t1 == t2 && promoted == promote,
            (MoveRecord::Drop { to: t1, piece, .. }, Move::Drop { to: t2, piece_type }) => {
                t1 == t2 && piece.piece_type == *piece_type
            }
            _ => false,
        }
    }
}

/// Helper: Convert file index to Japanese numeral.
fn file_to_japanese(file: u8) -> &'static str {
    match file + 1 {
        1 => "１",
        2 => "２",
        3 => "３",
        4 => "４",
        5 => "５",
        _ => "?",
    }
}

/// Helper: Convert rank index to Japanese kanji.
fn rank_to_kanji(rank: u8) -> &'static str {
    match rank {
        0 => "一",
        1 => "二",
        2 => "三",
        3 => "四",
        4 => "五",
        _ => "?",
    }
}

/// Helper: Convert piece type to kanji for display.
fn piece_type_to_kanji(pt: PieceType) -> &'static str {
    match pt {
        PieceType::King => "玉",
        PieceType::Rook => "飛",
        PieceType::Bishop => "角",
        PieceType::Gold => "金",
        PieceType::Silver => "銀",
        PieceType::Pawn => "歩",
        PieceType::ProRook => "龍",
        PieceType::ProBishop => "馬",
        PieceType::ProSilver => "成銀",
        PieceType::ProPawn => "と",
        // These should not appear in Mini Shogi
        PieceType::Knight | PieceType::Lance | PieceType::ProKnight | PieceType::ProLance => "?",
    }
}

/// Helper: Convert piece type to SFEN character.
fn piece_type_to_sfen_char(pt: PieceType) -> char {
    match pt {
        PieceType::King => 'K',
        PieceType::Rook | PieceType::ProRook => 'R',
        PieceType::Bishop | PieceType::ProBishop => 'B',
        PieceType::Gold => 'G',
        PieceType::Silver | PieceType::ProSilver => 'S',
        PieceType::Pawn | PieceType::ProPawn => 'P',
        PieceType::Knight | PieceType::ProKnight => 'N',
        PieceType::Lance | PieceType::ProLance => 'L',
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_sfen_normal() {
        let mv = Move::from_sfen("1a2a").unwrap();
        match mv {
            Move::Normal { from, to, promote } => {
                assert_eq!(0, from.file());
                assert_eq!(0, from.rank());
                assert_eq!(1, to.file());
                assert_eq!(0, to.rank());
                assert!(!promote);
            }
            _ => panic!("Expected normal move"),
        }
    }

    #[test]
    fn from_sfen_promotion() {
        let mv = Move::from_sfen("2d1e+").unwrap();
        match mv {
            Move::Normal { promote, .. } => assert!(promote),
            _ => panic!("Expected normal move"),
        }
    }

    #[test]
    fn from_sfen_drop() {
        let mv = Move::from_sfen("P*3c").unwrap();
        match mv {
            Move::Drop { to, piece_type } => {
                assert_eq!(PieceType::Pawn, piece_type);
                assert_eq!(2, to.file());
                assert_eq!(2, to.rank());
            }
            _ => panic!("Expected drop move"),
        }
    }

    #[test]
    fn from_sfen_invalid() {
        assert!(Move::from_sfen("").is_none());
        assert!(Move::from_sfen("1a").is_none());
        assert!(Move::from_sfen("6a1a").is_none()); // Invalid file
        assert!(Move::from_sfen("1f1a").is_none()); // Invalid rank
        assert!(Move::from_sfen("N*3c").is_none()); // Knight not valid in Mini Shogi
        assert!(Move::from_sfen("L*3c").is_none()); // Lance not valid in Mini Shogi
    }

    #[test]
    fn display() {
        let sq1 = Square::new(0, 0).unwrap();
        let sq2 = Square::new(1, 1).unwrap();

        let mv = Move::Normal {
            from: sq1,
            to: sq2,
            promote: false,
        };
        assert_eq!("1a2b", mv.to_string());

        let mv_promo = Move::Normal {
            from: sq1,
            to: sq2,
            promote: true,
        };
        assert_eq!("1a2b+", mv_promo.to_string());

        let drop = Move::Drop {
            to: sq2,
            piece_type: PieceType::Pawn,
        };
        assert_eq!("P*2b", drop.to_string());
    }

    #[test]
    fn to_japanese() {
        let sq1 = Square::new(2, 2).unwrap(); // 3c
        let sq2 = Square::new(2, 3).unwrap(); // 3d

        let mv = Move::Normal {
            from: sq1,
            to: sq2,
            promote: false,
        };
        assert_eq!("☗３四歩", mv.to_japanese(PieceType::Pawn, Color::Black));

        let drop = Move::Drop {
            to: sq2,
            piece_type: PieceType::Gold,
        };
        assert_eq!("☖３四金打", drop.to_japanese(PieceType::Gold, Color::White));
    }

    #[test]
    fn move_properties() {
        let sq1 = Square::new(0, 0).unwrap();
        let sq2 = Square::new(1, 1).unwrap();

        let normal = Move::Normal {
            from: sq1,
            to: sq2,
            promote: false,
        };
        assert_eq!(sq2, normal.to());
        assert!(!normal.is_drop());
        assert!(!normal.is_promotion());

        let promo = Move::Normal {
            from: sq1,
            to: sq2,
            promote: true,
        };
        assert!(promo.is_promotion());

        let drop = Move::Drop {
            to: sq2,
            piece_type: PieceType::Silver,
        };
        assert!(drop.is_drop());
        assert!(!drop.is_promotion());
    }
}
