//! Move recording and notation formatting.
//!
//! This module provides types for recording moves made during a game
//! and formatting them in various notation styles.

use crate::color::Color;
use crate::moves::StandardMove as Move;
use crate::piece::Piece;
use crate::piece_type::PieceType;
use crate::square::StandardSquare as Square;

/// MoveRecord stores information necessary to undo the move.
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
    /// Converts the move into SFEN formatted string.
    pub fn to_sfen(&self) -> String {
        match *self {
            MoveRecord::Normal {
                from, to, promoted, ..
            } => format!("{}{}{}", from, to, if promoted { "+" } else { "" }),
            MoveRecord::Drop {
                to,
                piece: Piece { piece_type, .. },
            } => format!("{}*{}", piece_type.to_string().to_uppercase(), to),
        }
    }

    /// Formats the move in Japanese notation.
    ///
    /// Japanese notation format: ☗７六歩 (black pawn to 7f)
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::{Color, Piece, PieceType, Move, Position};
    /// use shogi::bitboard::Factory as BBFactory;
    /// use shogi::square::consts::*;
    ///
    /// BBFactory::init();
    /// let mut pos = Position::new();
    /// pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();
    ///
    /// // Make a move
    /// let mv = Move::Normal { from: SQ_7G, to: SQ_7F, promote: false };
    /// pos.make_move(mv).unwrap();
    ///
    /// // Format the move record
    /// let record = &pos.move_history()[0];
    /// assert_eq!("☗７六歩", record.to_japanese());
    /// ```
    pub fn to_japanese(&self) -> String {
        match *self {
            MoveRecord::Normal {
                to,
                ref placed,
                promoted,
                ..
            } => {
                let symbol = if placed.color == Color::Black {
                    "☗"
                } else {
                    "☖"
                };

                // Get the piece type before promotion for display
                let piece_type = if promoted {
                    placed.piece_type.unpromote().unwrap_or(placed.piece_type)
                } else {
                    placed.piece_type
                };

                let file_str = Self::file_to_japanese(to.file());
                let rank_str = Self::rank_to_kanji(to.rank());
                let piece_str = Self::piece_type_to_japanese(piece_type);

                if promoted {
                    format!("{}{}{}{}成", symbol, file_str, rank_str, piece_str)
                } else {
                    format!("{}{}{}{}", symbol, file_str, rank_str, piece_str)
                }
            }
            MoveRecord::Drop { to, ref piece } => {
                let symbol = if piece.color == Color::Black {
                    "☗"
                } else {
                    "☖"
                };

                let file_str = Self::file_to_japanese(to.file());
                let rank_str = Self::rank_to_kanji(to.rank());
                let piece_str = Self::piece_type_to_japanese(piece.piece_type);

                format!("{}{}{}{}打", symbol, file_str, rank_str, piece_str)
            }
        }
    }

    /// Formats the move in Japanese notation with source square.
    ///
    /// This is useful for disambiguation when multiple pieces can move to the same square.
    pub fn to_japanese_detailed(&self) -> String {
        match *self {
            MoveRecord::Normal {
                from,
                to,
                ref placed,
                promoted,
                ..
            } => {
                let symbol = if placed.color == Color::Black {
                    "☗"
                } else {
                    "☖"
                };

                let piece_type = if promoted {
                    placed.piece_type.unpromote().unwrap_or(placed.piece_type)
                } else {
                    placed.piece_type
                };

                let to_file = Self::file_to_japanese(to.file());
                let to_rank = Self::rank_to_kanji(to.rank());
                let from_file = Self::file_to_japanese(from.file());
                let from_rank = Self::rank_to_kanji(from.rank());
                let piece_str = Self::piece_type_to_japanese(piece_type);

                if promoted {
                    format!(
                        "{}{}{}{}({}{})成",
                        symbol, to_file, to_rank, piece_str, from_file, from_rank
                    )
                } else {
                    format!(
                        "{}{}{}{}({}{})",
                        symbol, to_file, to_rank, piece_str, from_file, from_rank
                    )
                }
            }
            MoveRecord::Drop { to, ref piece } => {
                let symbol = if piece.color == Color::Black {
                    "☗"
                } else {
                    "☖"
                };

                let file_str = Self::file_to_japanese(to.file());
                let rank_str = Self::rank_to_kanji(to.rank());
                let piece_str = Self::piece_type_to_japanese(piece.piece_type);

                format!("{}{}{}{}打", symbol, file_str, rank_str, piece_str)
            }
        }
    }

    // Helper functions for Japanese notation
    fn file_to_japanese(file: u8) -> &'static str {
        // File 0 = SFEN file 1 = Japanese １
        // File 8 = SFEN file 9 = Japanese ９
        match file + 1 {
            1 => "１",
            2 => "２",
            3 => "３",
            4 => "４",
            5 => "５",
            6 => "６",
            7 => "７",
            8 => "８",
            9 => "９",
            _ => "?",
        }
    }

    fn rank_to_kanji(rank: u8) -> &'static str {
        match rank {
            0 => "一",
            1 => "二",
            2 => "三",
            3 => "四",
            4 => "五",
            5 => "六",
            6 => "七",
            7 => "八",
            8 => "九",
            _ => "?",
        }
    }

    fn piece_type_to_japanese(pt: PieceType) -> &'static str {
        match pt {
            PieceType::Pawn => "歩",
            PieceType::Lance => "香",
            PieceType::Knight => "桂",
            PieceType::Silver => "銀",
            PieceType::Gold => "金",
            PieceType::Bishop => "角",
            PieceType::Rook => "飛",
            PieceType::King => "玉",
            PieceType::ProPawn => "と",
            PieceType::ProLance => "成香",
            PieceType::ProKnight => "成桂",
            PieceType::ProSilver => "成銀",
            PieceType::ProBishop => "馬",
            PieceType::ProRook => "龍",
        }
    }

    /// Formats the move in the specified notation.
    pub fn to_notation(&self, format: NotationFormat) -> String {
        match format {
            NotationFormat::Japanese => self.to_japanese(),
            NotationFormat::Hosking => self.to_hosking(),
            NotationFormat::Hodges => self.to_hodges(),
        }
    }

    /// Formats the move in Hosking notation.
    ///
    /// Hosking notation format: P76, S*55, G83+
    /// - Piece letter (P, L, N, S, G, B, R, K)
    /// - File 1-9 (SFEN file number)
    /// - Rank 1-9 (1=top, 9=bottom)
    /// - + for promotion, * for drop
    pub fn to_hosking(&self) -> String {
        match *self {
            MoveRecord::Normal {
                to,
                ref placed,
                promoted,
                ..
            } => {
                let piece = Self::piece_type_to_sfen(placed.piece_type);
                let file = to.file() + 1; // Convert 0-indexed to SFEN file (1-9)
                let rank = to.rank() + 1; // Convert 0-indexed to rank number (1-9)
                if promoted {
                    format!("{}{}{}+", piece, file, rank)
                } else {
                    format!("{}{}{}", piece, file, rank)
                }
            }
            MoveRecord::Drop { to, ref piece } => {
                let piece_char = Self::piece_type_to_sfen(piece.piece_type);
                let file = to.file() + 1;
                let rank = to.rank() + 1;
                format!("{}*{}{}", piece_char, file, rank)
            }
        }
    }

    /// Formats the move in Hodges notation.
    ///
    /// Hodges notation format: P7f, S*5e, G8c+
    /// - Piece letter (P, L, N, S, G, B, R, K)
    /// - File 1-9 (SFEN file number)
    /// - Rank a-i (letters)
    /// - + for promotion, * for drop
    pub fn to_hodges(&self) -> String {
        match *self {
            MoveRecord::Normal {
                to,
                ref placed,
                promoted,
                ..
            } => {
                let piece = Self::piece_type_to_sfen(placed.piece_type);
                let file = to.file() + 1; // Convert 0-indexed to SFEN file (1-9)
                let rank_letter = (b'a' + to.rank()) as char;
                if promoted {
                    format!("{}{}{}+", piece, file, rank_letter)
                } else {
                    format!("{}{}{}", piece, file, rank_letter)
                }
            }
            MoveRecord::Drop { to, ref piece } => {
                let piece_char = Self::piece_type_to_sfen(piece.piece_type);
                let file = to.file() + 1;
                let rank_letter = (b'a' + to.rank()) as char;
                format!("{}*{}{}", piece_char, file, rank_letter)
            }
        }
    }

    /// Convert PieceType to SFEN letter (uppercase).
    fn piece_type_to_sfen(pt: PieceType) -> &'static str {
        match pt {
            PieceType::Pawn | PieceType::ProPawn => "P",
            PieceType::Lance | PieceType::ProLance => "L",
            PieceType::Knight | PieceType::ProKnight => "N",
            PieceType::Silver | PieceType::ProSilver => "S",
            PieceType::Gold => "G",
            PieceType::Bishop | PieceType::ProBishop => "B",
            PieceType::Rook | PieceType::ProRook => "R",
            PieceType::King => "K",
        }
    }
}

/// Notation format for move display.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotationFormat {
    /// Japanese: ☗７六歩成 (Symbol + fullwidth file + kanji rank + piece + 成/打)
    Japanese,
    /// Hosking: P76+ (Piece + file + rank + promotion)
    /// Files 9→1, Ranks 1→9 (numeric)
    Hosking,
    /// Hodges: P7f+ (Piece + file + rank_letter + promotion)
    /// Files 9→1, Ranks a→i (letters)
    Hodges,
}

impl PartialEq<Move> for MoveRecord {
    fn eq(&self, other: &Move) -> bool {
        match (self, other) {
            (
                &MoveRecord::Normal {
                    from: f1,
                    to: t1,
                    promoted,
                    ..
                },
                &Move::Normal {
                    from: f2,
                    to: t2,
                    promote,
                },
            ) => f1 == f2 && t1 == t2 && promote == promoted,
            (&MoveRecord::Drop { to: t1, piece, .. }, &Move::Drop { to: t2, piece_type }) => {
                t1 == t2 && piece.piece_type == piece_type
            }
            _ => false,
        }
    }
}
