use crate::square::Square;
use crate::{Color, PieceType};
use std::fmt;
use std::str::FromStr;

/// Returns the Japanese kanji for a rank number (0-indexed, a=0)
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

/// Returns the full-width numeral for a file number (0-indexed, where file 0 = '1' in SFEN)
fn file_to_japanese(file: u8, _width: u8) -> &'static str {
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

/// Returns the Japanese name for a piece type
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

/// Returns the player symbol (sente/gote indicator)
fn color_symbol(c: Color) -> &'static str {
    match c {
        Color::Black => "☗",
        Color::White => "☖",
    }
}

/// Represents a move which either is a normal move or a drop move.
///
/// The board dimensions are specified as const generic parameters:
/// - `W`: Width (number of files)
/// - `H`: Height (number of ranks)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Move<const W: u8, const H: u8> {
    Normal {
        from: Square<W, H>,
        to: Square<W, H>,
        promote: bool,
    },
    Drop {
        to: Square<W, H>,
        piece_type: PieceType,
    },
}

/// Type alias for standard 9×9 Shogi moves.
pub type StandardMove = Move<9, 9>;

/// Type alias for 5×5 Mini Shogi moves.
pub type MiniMove = Move<5, 5>;

impl<const W: u8, const H: u8> Move<W, H> {
    /// Formats the move in Japanese notation.
    ///
    /// # Arguments
    /// * `piece_type` - The type of piece being moved (for Normal moves) or dropped
    /// * `color` - The color of the player making the move
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::{Color, Move, PieceType};
    /// use shogi::square::consts::*;
    ///
    /// let mv = Move::Normal { from: SQ_7G, to: SQ_7F, promote: false };
    /// assert_eq!("☗７六歩", mv.to_japanese(PieceType::Pawn, Color::Black));
    ///
    /// let drop = Move::Drop { to: SQ_5E, piece_type: PieceType::Silver };
    /// assert_eq!("☗５五銀打", drop.to_japanese(PieceType::Silver, Color::Black));
    /// ```
    pub fn to_japanese(&self, piece_type: PieceType, color: Color) -> String {
        let symbol = color_symbol(color);

        match *self {
            Move::Normal { to, promote, .. } => {
                let file_str = file_to_japanese(to.file(), W);
                let rank_str = rank_to_kanji(to.rank());

                if promote {
                    // Show promotion with 成
                    format!(
                        "{}{}{}{}成",
                        symbol,
                        file_str,
                        rank_str,
                        piece_type_to_japanese(piece_type)
                    )
                } else {
                    format!(
                        "{}{}{}{}",
                        symbol,
                        file_str,
                        rank_str,
                        piece_type_to_japanese(piece_type)
                    )
                }
            }
            Move::Drop { to, piece_type: pt } => {
                let file_str = file_to_japanese(to.file(), W);
                let rank_str = rank_to_kanji(to.rank());

                format!(
                    "{}{}{}{}打",
                    symbol,
                    file_str,
                    rank_str,
                    piece_type_to_japanese(pt)
                )
            }
        }
    }

    /// Formats the move in Japanese notation with additional context.
    ///
    /// This version includes the source square for disambiguation when multiple
    /// pieces of the same type can move to the same square.
    ///
    /// # Arguments
    /// * `piece_type` - The type of piece being moved
    /// * `color` - The color of the player making the move
    /// * `include_from` - Whether to include the source square
    pub fn to_japanese_detailed(
        &self,
        piece_type: PieceType,
        color: Color,
        include_from: bool,
    ) -> String {
        let symbol = color_symbol(color);

        match *self {
            Move::Normal {
                from, to, promote, ..
            } => {
                let to_file = file_to_japanese(to.file(), W);
                let to_rank = rank_to_kanji(to.rank());

                let from_info = if include_from {
                    format!(
                        "({}{})",
                        file_to_japanese(from.file(), W),
                        rank_to_kanji(from.rank())
                    )
                } else {
                    String::new()
                };

                if promote {
                    format!(
                        "{}{}{}{}{}成",
                        symbol,
                        to_file,
                        to_rank,
                        piece_type_to_japanese(piece_type),
                        from_info
                    )
                } else {
                    format!(
                        "{}{}{}{}{}",
                        symbol,
                        to_file,
                        to_rank,
                        piece_type_to_japanese(piece_type),
                        from_info
                    )
                }
            }
            Move::Drop { to, piece_type: pt } => {
                let file_str = file_to_japanese(to.file(), W);
                let rank_str = rank_to_kanji(to.rank());

                format!(
                    "{}{}{}{}打",
                    symbol,
                    file_str,
                    rank_str,
                    piece_type_to_japanese(pt)
                )
            }
        }
    }

    /// Creates a new instance of `Move` from SFEN formatted string.
    pub fn from_sfen(s: &str) -> Option<Self> {
        if s.len() != 4 && (s.len() != 5 || s.chars().nth(4).unwrap() != '+') {
            return None;
        }

        let first = s.chars().next().unwrap();
        if first.is_ascii_digit() {
            if let Some(from) = Square::from_sfen(&s[0..2]) {
                if let Some(to) = Square::from_sfen(&s[2..4]) {
                    let promote = s.len() == 5;

                    return Some(Move::Normal { from, to, promote });
                }
            }

            return None;
        } else if first.is_uppercase() && s.chars().nth(1).unwrap() == '*' {
            if let Some(piece_type) = first.to_lowercase().next().and_then(PieceType::from_sfen) {
                if let Some(to) = Square::from_sfen(&s[2..4]) {
                    return Some(Move::Drop { to, piece_type });
                }
            }

            return None;
        }

        None
    }
}

impl<const W: u8, const H: u8> fmt::Display for Move<W, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Move::Normal { from, to, promote } => {
                write!(f, "{}{}{}", from, to, if promote { "+" } else { "" })
            }
            Move::Drop { to, piece_type } => {
                write!(f, "{}*{}", piece_type.to_string().to_uppercase(), to)
            }
        }
    }
}

/// Error type for parsing a move from SFEN notation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseMoveError;

impl fmt::Display for ParseMoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid move notation")
    }
}

impl std::error::Error for ParseMoveError {}

impl<const W: u8, const H: u8> FromStr for Move<W, H> {
    type Err = ParseMoveError;

    /// Parses a move from SFEN notation.
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Move;
    /// use shogi::square::consts::*;
    ///
    /// let mv: Move = "7g7f".parse().unwrap();
    /// assert!(matches!(mv, Move::Normal { from: SQ_7G, to: SQ_7F, promote: false }));
    ///
    /// let mv: Move = "7g7f+".parse().unwrap();
    /// assert!(matches!(mv, Move::Normal { from: SQ_7G, to: SQ_7F, promote: true }));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_sfen(s).ok_or(ParseMoveError)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::square::consts::*;

    #[test]
    fn from_sfen() {
        let ok_cases = [
            (
                "9a1i",
                StandardMove::Normal {
                    from: SQ_9A,
                    to: SQ_1I,
                    promote: false,
                },
            ),
            (
                "9a1i+",
                StandardMove::Normal {
                    from: SQ_9A,
                    to: SQ_1I,
                    promote: true,
                },
            ),
            (
                "S*5e",
                StandardMove::Drop {
                    to: SQ_5E,
                    piece_type: PieceType::Silver,
                },
            ),
        ];
        let ng_cases = [
            "9j1i", "9a1j", "9a1", "9aj", "j1i", "9a1i1", "9a1i-", "S+5e", "S 5e", "Z*5e", "S+9j",
        ];

        for (i, case) in ok_cases.iter().enumerate() {
            let m = StandardMove::from_sfen(case.0);
            assert!(m.is_some(), "failed at #{i}");
            assert_eq!(case.1, m.unwrap(), "failed at #{i}");
        }

        for (i, case) in ng_cases.iter().enumerate() {
            assert!(StandardMove::from_sfen(case).is_none(), "failed at #{i}");
        }
    }

    #[test]
    fn to_sfen() {
        let cases = [
            (
                "9a1i",
                StandardMove::Normal {
                    from: SQ_9A,
                    to: SQ_1I,
                    promote: false,
                },
            ),
            (
                "9a1i+",
                StandardMove::Normal {
                    from: SQ_9A,
                    to: SQ_1I,
                    promote: true,
                },
            ),
            (
                "S*5e",
                StandardMove::Drop {
                    to: SQ_5E,
                    piece_type: PieceType::Silver,
                },
            ),
        ];

        for (i, case) in cases.iter().enumerate() {
            assert_eq!(case.1.to_string(), case.0, "failed at #{i}");
        }
    }

    #[test]
    fn to_japanese_normal_move() {
        // 7g to 7f pawn move for black
        let mv = StandardMove::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        assert_eq!("☗７六歩", mv.to_japanese(PieceType::Pawn, Color::Black));

        // 3c to 3d pawn move for white
        let mv = StandardMove::Normal {
            from: SQ_3C,
            to: SQ_3D,
            promote: false,
        };
        assert_eq!("☖３四歩", mv.to_japanese(PieceType::Pawn, Color::White));

        // Promoting move
        let mv = StandardMove::Normal {
            from: SQ_7C,
            to: SQ_7B,
            promote: true,
        };
        assert_eq!("☗７二歩成", mv.to_japanese(PieceType::Pawn, Color::Black));
    }

    #[test]
    fn to_japanese_drop_move() {
        // Silver drop at 5e
        let mv = StandardMove::Drop {
            to: SQ_5E,
            piece_type: PieceType::Silver,
        };
        assert_eq!("☗５五銀打", mv.to_japanese(PieceType::Silver, Color::Black));

        // Pawn drop at 7d for white
        let mv = StandardMove::Drop {
            to: SQ_7D,
            piece_type: PieceType::Pawn,
        };
        assert_eq!("☖７四歩打", mv.to_japanese(PieceType::Pawn, Color::White));
    }

    #[test]
    fn to_japanese_detailed() {
        // 7g to 7f pawn move for black with source
        let mv = StandardMove::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        assert_eq!(
            "☗７六歩(７七)",
            mv.to_japanese_detailed(PieceType::Pawn, Color::Black, true)
        );

        // Without source
        assert_eq!(
            "☗７六歩",
            mv.to_japanese_detailed(PieceType::Pawn, Color::Black, false)
        );
    }
}
