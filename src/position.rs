use itertools::Itertools;
use std::fmt;
use std::fmt::Write as _;

use crate::bitboard::Factory as BBFactory;
use crate::bitboard::StandardBitboard as Bitboard;
use crate::color::Color;
use crate::error::{MoveError, SfenError};
use crate::hand::Hand;
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

/// Result of checking for impasse (jishogi) conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImpasseResult {
    /// Not an impasse situation (kings not both in promotion zone, or not enough pieces)
    None,
    /// Draw - both players meet declaration requirements
    Draw,
    /// One player wins by having more points
    Winner(Color),
}

/// Complete game status including all possible end conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameStatus {
    /// Game is in progress, no special conditions
    InProgress,
    /// Player is in check but has legal moves
    Check(Color),
    /// Checkmate - the specified color has been checkmated (their opponent wins)
    Checkmate(Color),
    /// Stalemate - the specified color has no legal moves but is not in check
    /// In shogi, stalemate means the stalemated player loses
    Stalemate(Color),
    /// Draw by repetition (sennichite) - same position 4 times
    Repetition,
    /// Perpetual check - the specified color loses for giving perpetual check
    PerpetualCheck(Color),
    /// Impasse result - None means draw, Some(Color) means that color wins
    Impasse(Option<Color>),
}

#[derive(Clone)]
struct PieceGrid([Option<Piece>; 81]);

impl PieceGrid {
    pub fn get(&self, sq: Square) -> &Option<Piece> {
        &self.0[sq.index()]
    }

    pub fn set(&mut self, sq: Square, pc: Option<Piece>) {
        self.0[sq.index()] = pc;
    }
}

impl fmt::Debug for PieceGrid {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "PieceGrid {{ ")?;

        for pc in self.0.iter() {
            write!(fmt, "{pc:?} ")?;
        }
        write!(fmt, "}}")
    }
}

/// Represents a state of the game.
///
/// # Examples
///
/// ```
/// use shogi::{Move, Position};
/// use shogi::bitboard::Factory as BBFactory;
/// use shogi::square::consts::*;
///
/// BBFactory::init();
/// let mut pos = Position::new();
/// pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();
///
/// let m = Move::Normal{from: SQ_7G, to: SQ_7F, promote: false};
/// pos.make_move(m).unwrap();
///
/// assert_eq!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f", pos.to_sfen());
/// ```
#[derive(Debug, Clone)]
pub struct Position {
    board: PieceGrid,
    hand: Hand,
    ply: u16,
    side_to_move: Color,
    move_history: Vec<MoveRecord>,
    sfen_history: Vec<(String, u16)>,
    /// Moves that have been undone (for redo and full history preservation)
    undone_moves: Vec<MoveRecord>,
    /// SFEN history entries that have been undone
    undone_sfen: Vec<(String, u16)>,
    occupied_bb: Bitboard,
    color_bb: [Bitboard; 2],
    type_bb: [Bitboard; 14],
}

/////////////////////////////////////////////////////////////////////////////
// Type implementation
/////////////////////////////////////////////////////////////////////////////

impl Position {
    /// Creates a new instance of `Position` with an empty board.
    pub fn new() -> Position {
        Default::default()
    }

    /////////////////////////////////////////////////////////////////////////
    // Accessors
    /////////////////////////////////////////////////////////////////////////

    /// Returns a piece at the given square.
    pub fn piece_at(&self, sq: Square) -> &Option<Piece> {
        self.board.get(sq)
    }

    /// Returns a bitboard containing pieces of the given player.
    pub fn player_bb(&self, c: Color) -> &Bitboard {
        &self.color_bb[c.index()]
    }

    /// Returns the number of the given piece in hand.
    pub fn hand(&self, p: Piece) -> u8 {
        self.hand.get(p)
    }

    /// Returns the side to make a move next.
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Returns the number of plies already completed by the current state.
    pub fn ply(&self) -> u16 {
        self.ply
    }

    /// Returns a history of all moves made since the beginning of the game.
    pub fn move_history(&self) -> &[MoveRecord] {
        &self.move_history
    }

    /// Returns moves that have been undone but not yet redone.
    ///
    /// These moves are preserved for game records and can be replayed with `redo_move()`.
    pub fn undone_moves(&self) -> &[MoveRecord] {
        &self.undone_moves
    }

    /// Returns the number of moves that can be redone.
    pub fn can_redo_count(&self) -> usize {
        self.undone_moves.len()
    }

    /// Returns true if there are undone moves that can be redone.
    pub fn can_redo(&self) -> bool {
        !self.undone_moves.is_empty()
    }

    /// Returns the complete game history including both active moves and undone moves.
    ///
    /// This is useful for preserving the full game record even after using `unmake_move`.
    /// The returned vector contains moves in order: active moves first, then undone moves in reverse order.
    pub fn full_history(&self) -> Vec<MoveRecord> {
        let mut history = self.move_history.clone();
        // Undone moves are stored in reverse order (LIFO), so we add them in reverse
        history.extend(self.undone_moves.iter().rev().cloned());
        history
    }

    /// Checks if a player with the given color can declare winning.
    ///
    /// See [the section 25 in 世界コンピュータ将棋選手権 大会ルール][csa] for more detail.
    ///
    /// [csa]: http://www2.computer-shogi.org/wcsc26/rule.pdf#page=9
    pub fn try_declare_winning(&self, c: Color) -> bool {
        if c != self.side_to_move {
            return false;
        }

        let king_pos = self.find_king(c);
        if king_pos.is_none() {
            return false;
        }

        let king_pos = king_pos.unwrap();
        if king_pos.relative_rank(c) >= 3 {
            return false;
        }

        let (mut point, count) =
            PieceType::iter()
                .filter(|&pt| pt != PieceType::King)
                .fold((0, 0), |accum, pt| {
                    let unit = match pt {
                        PieceType::Rook
                        | PieceType::Bishop
                        | PieceType::ProRook
                        | PieceType::ProBishop => 5,
                        _ => 1,
                    };

                    let bb = &(&self.type_bb[pt.index()] & &self.color_bb[c.index()])
                        & &BBFactory::promote_zone(c);
                    let count = bb.count() as u8;
                    let point = count * unit;

                    (accum.0 + point, accum.1 + count)
                });

        if count < 10 {
            return false;
        }

        point += PieceType::iter()
            .filter(|pt| pt.is_hand_piece())
            .fold(0, |acc, pt| {
                let num = self.hand.get(Piece {
                    piece_type: pt,
                    color: c,
                });
                let pp = match pt {
                    PieceType::Rook | PieceType::Bishop => 5,
                    _ => 1,
                };

                acc + num * pp
            });

        let lowerbound = match c {
            Color::Black => 28,
            Color::White => 27,
        };
        if point < lowerbound {
            return false;
        }

        if self.in_check(c) {
            return false;
        }

        true
    }

    /// Calculates the impasse (jishogi) points for the given color.
    ///
    /// Points are calculated as follows:
    /// - Major pieces (Rook, Bishop, promoted Rook, promoted Bishop): 5 points each
    /// - All other pieces: 1 point each
    ///
    /// This counts pieces in the promotion zone on the board plus pieces in hand.
    /// The king is not counted.
    pub fn impasse_points(&self, c: Color) -> u32 {
        // Points from pieces on board in promotion zone
        let board_points =
            PieceType::iter()
                .filter(|&pt| pt != PieceType::King)
                .fold(0u32, |accum, pt| {
                    let unit: u32 = match pt {
                        PieceType::Rook
                        | PieceType::Bishop
                        | PieceType::ProRook
                        | PieceType::ProBishop => 5,
                        _ => 1,
                    };

                    let bb = &(&self.type_bb[pt.index()] & &self.color_bb[c.index()])
                        & &BBFactory::promote_zone(c);
                    let count = bb.count() as u32;

                    accum + count * unit
                });

        // Points from pieces in hand
        let hand_points = PieceType::iter()
            .filter(|pt| pt.is_hand_piece())
            .fold(0u32, |acc, pt| {
                let num = self.hand.get(Piece {
                    piece_type: pt,
                    color: c,
                }) as u32;
                let pp = match pt {
                    PieceType::Rook | PieceType::Bishop => 5,
                    _ => 1,
                };

                acc + num * pp
            });

        board_points + hand_points
    }

    /// Counts the number of pieces (excluding king) in the promotion zone for the given color.
    pub fn pieces_in_promotion_zone(&self, c: Color) -> u32 {
        PieceType::iter()
            .filter(|&pt| pt != PieceType::King)
            .fold(0u32, |accum, pt| {
                let bb = &(&self.type_bb[pt.index()] & &self.color_bb[c.index()])
                    & &BBFactory::promote_zone(c);
                accum + bb.count() as u32
            })
    }

    /// Checks if the position is an impasse (jishogi) situation and determines the result.
    ///
    /// An impasse occurs when both kings have entered the opponent's territory (promotion zone)
    /// and a checkmate is unlikely. The winner is determined by counting points.
    ///
    /// Returns:
    /// - `ImpasseResult::None` if this is not an impasse situation
    /// - `ImpasseResult::Draw` if both players meet the minimum requirements
    /// - `ImpasseResult::Winner(color)` if one player wins by having more points
    ///
    /// Note: This checks the impasse conditions but does not verify all tournament rules.
    /// Tournament rules may vary (e.g., CSA rules require 10+ pieces in zone and 24+ points).
    pub fn check_impasse(&self) -> ImpasseResult {
        // Check if both kings are in their respective promotion zones
        let black_king = self.find_king(Color::Black);
        let white_king = self.find_king(Color::White);

        let black_in_zone = black_king
            .map(|sq| sq.relative_rank(Color::Black) < 3)
            .unwrap_or(false);
        let white_in_zone = white_king
            .map(|sq| sq.relative_rank(Color::White) < 3)
            .unwrap_or(false);

        // Not an impasse if both kings aren't in promotion zones
        if !black_in_zone || !white_in_zone {
            return ImpasseResult::None;
        }

        // Count pieces in promotion zone for each side (need 10+ for declaration)
        let black_pieces = self.pieces_in_promotion_zone(Color::Black);
        let white_pieces = self.pieces_in_promotion_zone(Color::White);

        // If neither player has enough pieces, not an impasse
        if black_pieces < 10 && white_pieces < 10 {
            return ImpasseResult::None;
        }

        // Calculate points
        let black_points = self.impasse_points(Color::Black);
        let white_points = self.impasse_points(Color::White);

        // Apply thresholds (Black needs 28+, White needs 27+)
        let black_qualifies = black_pieces >= 10 && black_points >= 28;
        let white_qualifies = white_pieces >= 10 && white_points >= 27;

        match (black_qualifies, white_qualifies) {
            (true, true) => {
                // Both qualify - higher points wins, or draw if equal
                if black_points > white_points {
                    ImpasseResult::Winner(Color::Black)
                } else if white_points > black_points {
                    ImpasseResult::Winner(Color::White)
                } else {
                    ImpasseResult::Draw
                }
            }
            (true, false) => ImpasseResult::Winner(Color::Black),
            (false, true) => ImpasseResult::Winner(Color::White),
            (false, false) => ImpasseResult::None,
        }
    }

    /// Checks if the king with the given color is in check.
    pub fn in_check(&self, c: Color) -> bool {
        if let Some(king_sq) = self.find_king(c) {
            self.is_attacked_by(king_sq, c.flip())
        } else {
            false
        }
    }

    /// Returns true if the specified color has at least one legal move.
    ///
    /// This includes both normal moves (piece movements) and drop moves.
    /// A move is legal if it doesn't leave the king in check.
    pub fn has_legal_moves(&self, c: Color) -> bool {
        // Check all pieces of the given color for legal moves
        for sq in Square::iter() {
            if let Some(pc) = *self.piece_at(sq) {
                if pc.color == c {
                    // Try all move candidates from this square
                    for to in self.move_candidates(sq, pc) {
                        // Try without promotion
                        let mv = Move::Normal {
                            from: sq,
                            to,
                            promote: false,
                        };
                        if self.is_legal_move_for(&mv, c) {
                            return true;
                        }

                        // Try with promotion if possible
                        let promo_zone = BBFactory::promote_zone(c);
                        if (promo_zone.contains(sq) || promo_zone.contains(to))
                            && pc.piece_type.promote().is_some()
                        {
                            let mv = Move::Normal {
                                from: sq,
                                to,
                                promote: true,
                            };
                            if self.is_legal_move_for(&mv, c) {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        // Check drop moves for all piece types in hand
        let hand_piece_types = [
            PieceType::Pawn,
            PieceType::Lance,
            PieceType::Knight,
            PieceType::Silver,
            PieceType::Gold,
            PieceType::Rook,
            PieceType::Bishop,
        ];

        for pt in hand_piece_types {
            let pc = Piece {
                piece_type: pt,
                color: c,
            };
            if self.hand(pc) > 0 {
                // Try dropping on each empty square
                for sq in Square::iter() {
                    if self.piece_at(sq).is_none() {
                        let mv = Move::Drop {
                            to: sq,
                            piece_type: pt,
                        };
                        if self.is_legal_move_for(&mv, c) {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Returns all legal moves for the side to move.
    ///
    /// This includes both normal moves (piece movements) and drop moves.
    /// A move is legal if it doesn't leave the king in check and follows all shogi rules.
    pub fn legal_moves(&self) -> Vec<Move> {
        self.legal_moves_for(self.side_to_move)
    }

    /// Returns all legal moves for the specified color.
    ///
    /// This includes both normal moves (piece movements) and drop moves.
    /// A move is legal if it doesn't leave the king in check and follows all shogi rules.
    pub fn legal_moves_for(&self, c: Color) -> Vec<Move> {
        let mut moves = Vec::new();

        // Get all piece moves
        for sq in Square::iter() {
            if let Some(pc) = *self.piece_at(sq) {
                if pc.color == c {
                    // Try all move candidates from this square
                    for to in self.move_candidates(sq, pc) {
                        // Try without promotion
                        let mv = Move::Normal {
                            from: sq,
                            to,
                            promote: false,
                        };
                        if self.is_legal_move_for(&mv, c) {
                            moves.push(mv);
                        }

                        // Try with promotion if possible
                        let promo_zone = BBFactory::promote_zone(c);
                        if (promo_zone.contains(sq) || promo_zone.contains(to))
                            && pc.piece_type.promote().is_some()
                        {
                            let mv = Move::Normal {
                                from: sq,
                                to,
                                promote: true,
                            };
                            if self.is_legal_move_for(&mv, c) {
                                moves.push(mv);
                            }
                        }
                    }
                }
            }
        }

        // Get all drop moves
        let hand_piece_types = [
            PieceType::Pawn,
            PieceType::Lance,
            PieceType::Knight,
            PieceType::Silver,
            PieceType::Gold,
            PieceType::Rook,
            PieceType::Bishop,
        ];

        for pt in hand_piece_types {
            let pc = Piece {
                piece_type: pt,
                color: c,
            };
            if self.hand(pc) > 0 {
                // Try dropping on each empty square
                for sq in Square::iter() {
                    if self.piece_at(sq).is_none() {
                        let mv = Move::Drop {
                            to: sq,
                            piece_type: pt,
                        };
                        if self.is_legal_move_for(&mv, c) {
                            moves.push(mv);
                        }
                    }
                }
            }
        }

        moves
    }

    /// Returns true if the specified color is in checkmate.
    ///
    /// Checkmate occurs when the king is in check and has no legal moves to escape.
    pub fn is_checkmate(&self, c: Color) -> bool {
        self.in_check(c) && !self.has_legal_moves(c)
    }

    /// Returns true if the specified color is in stalemate.
    ///
    /// Stalemate occurs when the player is NOT in check but has no legal moves.
    /// Note: In shogi, stalemate is a loss for the stalemated player (unlike chess).
    pub fn is_stalemate(&self, c: Color) -> bool {
        !self.in_check(c) && !self.has_legal_moves(c)
    }

    /// Checks if a move is legal (doesn't leave the king in check and follows all rules).
    ///
    /// This method clones the position, sets the appropriate side to move based on the
    /// move's piece color, attempts the move, and checks if the resulting position is valid.
    ///
    /// For normal moves, the color is inferred from the piece at the source square.
    /// For drop moves, the current side to move is assumed.
    pub fn is_legal_move(&self, mv: &Move) -> bool {
        let color = match mv {
            Move::Normal { from, .. } => {
                if let Some(pc) = *self.piece_at(*from) {
                    pc.color
                } else {
                    return false; // No piece at source square
                }
            }
            Move::Drop { .. } => self.side_to_move(),
        };

        self.is_legal_move_for(mv, color)
    }

    /// Checks if a move is legal for a specific color.
    ///
    /// This is useful when checking moves for a color that is not the current side to move.
    pub fn is_legal_move_for(&self, mv: &Move, color: Color) -> bool {
        let mut test_pos = self.clone();
        test_pos.side_to_move = color;
        test_pos.make_move(*mv).is_ok()
    }

    /// Get the current game status.
    ///
    /// This checks relevant conditions and returns the appropriate status for the
    /// side to move. Note that repetition and perpetual check are detected during
    /// `make_move()` and will return errors - this method checks the current position.
    pub fn game_status(&self) -> GameStatus {
        let stm = self.side_to_move();

        // Check for checkmate first
        if self.is_checkmate(stm) {
            return GameStatus::Checkmate(stm);
        }

        // Check for stalemate
        if self.is_stalemate(stm) {
            return GameStatus::Stalemate(stm);
        }

        // Check if in check (but not checkmate)
        if self.in_check(stm) {
            return GameStatus::Check(stm);
        }

        // Check for repetition (4-fold repetition = sennichite)
        if self.is_repetition() {
            // Check if it's perpetual check
            if let Some(checker) = self.perpetual_check_color() {
                return GameStatus::PerpetualCheck(checker);
            }
            return GameStatus::Repetition;
        }

        GameStatus::InProgress
    }

    /// Returns true if the game is over (checkmate, stalemate, repetition, etc.)
    pub fn is_game_over(&self) -> bool {
        !matches!(
            self.game_status(),
            GameStatus::InProgress | GameStatus::Check(_)
        )
    }

    /// Returns the winner if the game is over, None if draw or ongoing.
    pub fn winner(&self) -> Option<Color> {
        match self.game_status() {
            GameStatus::Checkmate(c) => Some(c.flip()), // Opponent of checkmated player wins
            GameStatus::Stalemate(c) => Some(c.flip()), // Opponent of stalemated player wins
            GameStatus::PerpetualCheck(c) => Some(c.flip()), // Opponent of perpetual checker wins
            GameStatus::Impasse(Some(c)) => Some(c),
            _ => None,
        }
    }

    /// Check if the position has occurred 4 times (sennichite).
    fn is_repetition(&self) -> bool {
        if self.sfen_history.len() < 4 {
            return false;
        }

        let current_sfen = self.generate_sfen().split(' ').take(3).join(" ");
        let count = self
            .sfen_history
            .iter()
            .filter(|(sfen, _)| sfen == &current_sfen)
            .count();

        count >= 3 // Current position + 3 in history = 4-fold
    }

    /// If there's a 4-fold repetition, determine if one side was giving perpetual check.
    /// Returns the color that was giving perpetual check, if any.
    fn perpetual_check_color(&self) -> Option<Color> {
        if self.sfen_history.len() < 4 {
            return None;
        }

        // Check if one side has been continuously checking
        // The sfen_history stores (sfen, continuous_check_count)
        // If one side has high continuous check count, they're the perpetual checker

        // Look at the last few positions
        let recent: Vec<_> = self
            .sfen_history
            .iter()
            .rev()
            .take(8)
            .collect();

        // Check continuous check counts
        // If checks have been alternating with high counts, one side is perpetually checking
        for (_, check_count) in recent.iter() {
            if *check_count >= 4 {
                // The side that was NOT being checked is the perpetual checker
                // This is the opponent of the side to move at that position
                return Some(self.side_to_move().flip());
            }
        }

        None
    }

    /// Returns the position of the king with the given color.
    pub fn find_king(&self, c: Color) -> Option<Square> {
        let mut bb = &self.type_bb[PieceType::King.index()] & &self.color_bb[c.index()];
        if bb.is_any() {
            Some(bb.pop())
        } else {
            None
        }
    }

    /// Sets a piece at the given square.
    fn set_piece(&mut self, sq: Square, p: Option<Piece>) {
        self.board.set(sq, p);
    }

    /// Checks if the given square is attacked by any piece of the specified color.
    ///
    /// # Arguments
    /// * `sq` - The square to check
    /// * `c` - The color of the attacking pieces to check for
    ///
    /// # Returns
    /// `true` if any piece of color `c` attacks the square `sq`
    pub fn is_attacked_by(&self, sq: Square, c: Color) -> bool {
        PieceType::iter().any(|pt| self.get_attackers_of_type(pt, sq, c).is_any())
    }

    /// Returns a bitboard of all pieces of the specified color that attack the given square.
    ///
    /// # Arguments
    /// * `sq` - The square to check
    /// * `c` - The color of the attacking pieces
    ///
    /// # Returns
    /// A `Bitboard` with bits set for each piece of color `c` that attacks `sq`
    pub fn attackers(&self, sq: Square, c: Color) -> Bitboard {
        let mut result = Bitboard::empty();
        for pt in PieceType::iter() {
            result |= &self.get_attackers_of_type(pt, sq, c);
        }
        result
    }

    fn get_attackers_of_type(&self, pt: PieceType, sq: Square, c: Color) -> Bitboard {
        let bb = &self.type_bb[pt.index()] & &self.color_bb[c.index()];

        if bb.is_empty() {
            return bb;
        }

        let attack_pc = Piece {
            piece_type: pt,
            color: c,
        };

        &bb & &self.move_candidates(sq, attack_pc.flip())
    }

    fn log_position(&mut self) {
        // TODO: SFEN string is used to represent a state of position, but any transformation which uniquely distinguish positions can be used here.
        // Consider light-weight option if generating SFEN string for each move is time-consuming.
        let sfen = self.generate_sfen().split(' ').take(3).join(" ");
        let in_check = self.in_check(self.side_to_move());

        let continuous_check = if in_check {
            let past = if self.sfen_history.len() >= 2 {
                let record = self.sfen_history.get(self.sfen_history.len() - 2).unwrap();
                record.1
            } else {
                0
            };
            past + 1
        } else {
            0
        };

        self.sfen_history.push((sfen, continuous_check));
    }

    /////////////////////////////////////////////////////////////////////////
    // Making a move
    /////////////////////////////////////////////////////////////////////////

    /// Makes the given move. Returns `Err` if the move is invalid or any special condition is met.
    ///
    /// Note: Making a move after calling `unmake_move` will clear the undo stack (branching).
    pub fn make_move(&mut self, m: Move) -> Result<(), MoveError> {
        // Clear undone moves when branching (making a new move after undo)
        self.undone_moves.clear();
        self.undone_sfen.clear();

        let res = match m {
            Move::Normal { from, to, promote } => self.make_normal_move(from, to, promote)?,
            Move::Drop { to, piece_type } => self.make_drop_move(to, piece_type)?,
        };

        self.move_history.push(res);
        Ok(())
    }

    fn make_normal_move(
        &mut self,
        from: Square,
        to: Square,
        promoted: bool,
    ) -> Result<MoveRecord, MoveError> {
        let stm = self.side_to_move();
        let opponent = stm.flip();

        let moved = self
            .piece_at(from)
            .ok_or(MoveError::Inconsistent("No piece found"))?;

        let captured = *self.piece_at(to);

        if moved.color != stm {
            return Err(MoveError::Inconsistent(
                "The piece is not for the side to move",
            ));
        }

        // Check if promotion is valid (piece must be in or entering promotion zone)
        let promo_zone = BBFactory::promote_zone(stm);
        if promoted && !promo_zone.contains(from) && !promo_zone.contains(to) {
            return Err(MoveError::Inconsistent("The piece cannot promote"));
        }

        if !self.move_candidates(from, moved).any(|sq| sq == to) {
            return Err(MoveError::Inconsistent("The piece cannot move to there"));
        }

        if !promoted && !moved.is_placeable_at(to) {
            return Err(MoveError::NonMovablePiece);
        }

        let placed = if promoted {
            match moved.promote() {
                Some(promoted) => promoted,
                None => return Err(MoveError::Inconsistent("This type of piece cannot promote")),
            }
        } else {
            moved
        };

        self.set_piece(from, None);
        self.set_piece(to, Some(placed));
        self.occupied_bb ^= from;
        self.occupied_bb ^= to;
        self.type_bb[moved.piece_type.index()] ^= from;
        self.type_bb[placed.piece_type.index()] ^= to;
        self.color_bb[moved.color.index()] ^= from;
        self.color_bb[placed.color.index()] ^= to;

        if let Some(ref cap) = captured {
            self.occupied_bb ^= to;
            self.type_bb[cap.piece_type.index()] ^= to;
            self.color_bb[cap.color.index()] ^= to;
            let pc = cap.flip();
            let pc = match pc.unpromote() {
                Some(unpromoted) => unpromoted,
                None => pc,
            };
            self.hand.increment(pc);
        }

        if self.in_check(stm) {
            // Undo-ing the move.
            self.set_piece(from, Some(moved));
            self.set_piece(to, captured);
            self.occupied_bb ^= from;
            self.occupied_bb ^= to;
            self.type_bb[moved.piece_type.index()] ^= from;
            self.type_bb[placed.piece_type.index()] ^= to;
            self.color_bb[moved.color.index()] ^= from;
            self.color_bb[placed.color.index()] ^= to;

            if let Some(ref cap) = captured {
                self.occupied_bb ^= to;
                self.type_bb[cap.piece_type.index()] ^= to;
                self.color_bb[cap.color.index()] ^= to;
                let pc = cap.flip();
                let pc = match pc.unpromote() {
                    Some(unpromoted) => unpromoted,
                    None => pc,
                };
                self.hand.decrement(pc);
            }

            return Err(MoveError::InCheck);
        }

        self.side_to_move = opponent;
        self.ply += 1;

        self.log_position();
        self.detect_repetition()?;

        Ok(MoveRecord::Normal {
            from,
            to,
            placed,
            captured,
            promoted,
        })
    }

    fn make_drop_move(&mut self, to: Square, pt: PieceType) -> Result<MoveRecord, MoveError> {
        let stm = self.side_to_move();
        let opponent = stm.flip();

        if self.piece_at(to).is_some() {
            return Err(MoveError::Inconsistent("There is already a piece in `to`"));
        }

        let pc = Piece {
            piece_type: pt,
            color: stm,
        };

        if self.hand(pc) == 0 {
            return Err(MoveError::Inconsistent("The piece is not in the hand"));
        }

        if !pc.is_placeable_at(to) {
            return Err(MoveError::NonMovablePiece);
        }

        if pc.piece_type == PieceType::Pawn {
            // Nifu check.
            for i in 0..9 {
                if let Some(fp) = *self.piece_at(Square::new(to.file(), i).unwrap()) {
                    if fp == pc {
                        return Err(MoveError::Nifu);
                    }
                }
            }

            // Uchifuzume check.
            if let Some(king_sq) = to.shift(0, if stm == Color::Black { -1 } else { 1 }) {
                // Is the dropped pawn attacking the opponent's king?
                if let Some(
                    pc @ Piece {
                        piece_type: PieceType::King,
                        ..
                    },
                ) = *self.piece_at(king_sq)
                {
                    if pc.color == opponent {
                        // can any opponent's piece attack the dropped pawn?
                        let pinned = self.pinned_bb(opponent);

                        let not_attacked = PieceType::iter()
                            .filter(|&pt| pt != PieceType::King)
                            .flat_map(|pt| self.get_attackers_of_type(pt, to, opponent))
                            .all(|sq| (&pinned & sq).is_any());

                        if not_attacked {
                            // the dropped pawn may block bishop's moves
                            self.occupied_bb ^= to;
                            // can the opponent's king evade?
                            let is_attacked = |sq| {
                                if let Some(pc) = *self.piece_at(sq) {
                                    if pc.color == opponent {
                                        return true;
                                    }
                                }

                                self.is_attacked_by(sq, stm)
                            };
                            let uchifuzume = self.move_candidates(king_sq, pc).all(is_attacked);
                            self.occupied_bb ^= to;

                            if uchifuzume {
                                return Err(MoveError::Uchifuzume);
                            }
                        }
                    }
                }
            }
        }

        self.set_piece(to, Some(pc));
        self.occupied_bb ^= to;
        self.type_bb[pc.piece_type.index()] ^= to;
        self.color_bb[pc.color.index()] ^= to;

        if self.in_check(stm) {
            // Undo-ing the move.
            self.set_piece(to, None);
            self.occupied_bb ^= to;
            self.type_bb[pc.piece_type.index()] ^= to;
            self.color_bb[pc.color.index()] ^= to;
            return Err(MoveError::InCheck);
        }

        self.hand.decrement(pc);
        self.side_to_move = opponent;
        self.ply += 1;

        self.log_position();
        self.detect_repetition()?;

        Ok(MoveRecord::Drop { to, piece: pc })
    }

    /// Returns a list of squares at which a piece of the given color is pinned.
    pub fn pinned_bb(&self, c: Color) -> Bitboard {
        let ksq = self.find_king(c);
        if ksq.is_none() {
            return Bitboard::empty();
        }
        let ksq = ksq.unwrap();

        [
            (
                PieceType::Rook,
                BBFactory::rook_attack(ksq, &Bitboard::empty()),
            ),
            (
                PieceType::ProRook,
                BBFactory::rook_attack(ksq, &Bitboard::empty()),
            ),
            (
                PieceType::Bishop,
                BBFactory::bishop_attack(ksq, &Bitboard::empty()),
            ),
            (
                PieceType::ProBishop,
                BBFactory::bishop_attack(ksq, &Bitboard::empty()),
            ),
            (
                PieceType::Lance,
                BBFactory::lance_attack(c, ksq, &Bitboard::empty()),
            ),
        ]
        .iter()
        .fold(Bitboard::empty(), |mut accum, &(pt, ref mask)| {
            let bb = &(&self.type_bb[pt.index()] & &self.color_bb[c.flip().index()]) & mask;

            for psq in bb {
                let between = &BBFactory::between(ksq, psq) & &self.occupied_bb;
                if between.count() == 1 && (&between & &self.color_bb[c.index()]).is_any() {
                    accum |= &between;
                }
            }

            accum
        })
    }

    /// Undoes the last move, preserving it for potential redo or game record.
    ///
    /// The undone move is stored and can be:
    /// - Retrieved via `undone_moves()` for game records
    /// - Replayed via `redo_move()`
    ///
    /// Note: Making a new move after undo will clear the undone moves (branching).
    pub fn unmake_move(&mut self) -> Result<(), MoveError> {
        if self.move_history.is_empty() {
            // TODO: error?
            return Ok(());
        }

        let last = self.move_history.pop().unwrap();
        match last {
            MoveRecord::Normal {
                from,
                to,
                ref placed,
                ref captured,
                promoted,
            } => {
                if self.piece_at(from).is_some() {
                    // Re-push the move before returning error
                    self.move_history.push(MoveRecord::Normal {
                        from,
                        to,
                        placed: *placed,
                        captured: *captured,
                        promoted,
                    });
                    return Err(MoveError::Inconsistent(
                        "`from` of the move is filled by another piece",
                    ));
                }

                let moved = if promoted {
                    match placed.unpromote() {
                        Some(unpromoted) => unpromoted,
                        None => {
                            self.move_history.push(MoveRecord::Normal {
                                from,
                                to,
                                placed: *placed,
                                captured: *captured,
                                promoted,
                            });
                            return Err(MoveError::Inconsistent("Cannot unpromoted the piece"));
                        }
                    }
                } else {
                    *placed
                };
                if *self.piece_at(to) != Some(*placed) {
                    self.move_history.push(MoveRecord::Normal {
                        from,
                        to,
                        placed: *placed,
                        captured: *captured,
                        promoted,
                    });
                    return Err(MoveError::Inconsistent(
                        "Expected piece is not found in `to`",
                    ));
                }

                self.set_piece(from, Some(moved));
                self.set_piece(to, *captured);
                self.occupied_bb ^= from;
                self.occupied_bb ^= to;
                self.type_bb[moved.piece_type.index()] ^= from;
                self.type_bb[placed.piece_type.index()] ^= to;
                self.color_bb[moved.color.index()] ^= from;
                self.color_bb[placed.color.index()] ^= to;

                if let Some(ref cap) = *captured {
                    self.occupied_bb ^= to;
                    self.type_bb[cap.piece_type.index()] ^= to;
                    self.color_bb[cap.color.index()] ^= to;
                    let unpromoted_cap = cap.unpromote().unwrap_or(*cap);
                    self.hand.decrement(unpromoted_cap.flip());
                }

                // Store the undone move for potential redo
                self.undone_moves.push(MoveRecord::Normal {
                    from,
                    to,
                    placed: *placed,
                    captured: *captured,
                    promoted,
                });
            }
            MoveRecord::Drop { to, piece } => {
                if *self.piece_at(to) != Some(piece) {
                    self.move_history.push(MoveRecord::Drop { to, piece });
                    return Err(MoveError::Inconsistent(
                        "Expected piece is not found in `to`",
                    ));
                }

                self.set_piece(to, None);
                self.occupied_bb ^= to;
                self.type_bb[piece.piece_type.index()] ^= to;
                self.color_bb[piece.color.index()] ^= to;
                self.hand.increment(piece);

                // Store the undone move for potential redo
                self.undone_moves.push(MoveRecord::Drop { to, piece });
            }
        };

        self.side_to_move = self.side_to_move.flip();
        self.ply -= 1;

        // Store the undone sfen for potential redo
        if let Some(sfen) = self.sfen_history.pop() {
            self.undone_sfen.push(sfen);
        }

        Ok(())
    }

    /// Replays the last undone move.
    ///
    /// Returns the move that was replayed, or an error if there are no undone moves.
    /// This is useful for implementing redo functionality in a game interface.
    pub fn redo_move(&mut self) -> Result<MoveRecord, MoveError> {
        let last_undone = self
            .undone_moves
            .pop()
            .ok_or(MoveError::Inconsistent("No moves to redo"))?;

        // Convert MoveRecord back to Move and make it
        let m = match &last_undone {
            MoveRecord::Normal {
                from,
                to,
                promoted,
                ..
            } => Move::Normal {
                from: *from,
                to: *to,
                promote: *promoted,
            },
            MoveRecord::Drop { to, piece } => Move::Drop {
                to: *to,
                piece_type: piece.piece_type,
            },
        };

        // We need to not clear undone_moves when redoing, so call internal methods
        let res = match m {
            Move::Normal { from, to, promote } => self.make_normal_move(from, to, promote),
            Move::Drop { to, piece_type } => self.make_drop_move(to, piece_type),
        };

        match res {
            Ok(record) => {
                self.move_history.push(record.clone());
                // Restore the sfen from undone_sfen
                if let Some(sfen) = self.undone_sfen.pop() {
                    // Note: the sfen was already added by make_normal_move/make_drop_move via log_position
                    // We don't need to restore it separately, just pop to keep in sync
                    let _ = sfen;
                }
                Ok(record)
            }
            Err(e) => {
                // Put the undone move back if redo failed
                self.undone_moves.push(last_undone);
                Err(e)
            }
        }
    }

    /// Returns a list of squares to where the given piece at the given square can move.
    pub fn move_candidates(&self, sq: Square, p: Piece) -> Bitboard {
        let bb = match p.piece_type {
            PieceType::Rook => BBFactory::rook_attack(sq, &self.occupied_bb),
            PieceType::Bishop => BBFactory::bishop_attack(sq, &self.occupied_bb),
            PieceType::Lance => BBFactory::lance_attack(p.color, sq, &self.occupied_bb),
            PieceType::ProRook => {
                &BBFactory::rook_attack(sq, &self.occupied_bb)
                    | &BBFactory::attacks_from(PieceType::King, p.color, sq)
            }
            PieceType::ProBishop => {
                &BBFactory::bishop_attack(sq, &self.occupied_bb)
                    | &BBFactory::attacks_from(PieceType::King, p.color, sq)
            }
            PieceType::ProSilver
            | PieceType::ProKnight
            | PieceType::ProLance
            | PieceType::ProPawn => BBFactory::attacks_from(PieceType::Gold, p.color, sq),
            pt => BBFactory::attacks_from(pt, p.color, sq),
        };

        &bb & &!&self.color_bb[p.color.index()]
    }

    fn detect_repetition(&self) -> Result<(), MoveError> {
        if self.sfen_history.len() < 9 {
            return Ok(());
        }

        let cur = self.sfen_history.last().unwrap();

        let mut cnt = 0;
        for (i, entry) in self.sfen_history.iter().rev().enumerate() {
            if entry.0 == cur.0 {
                cnt += 1;

                if cnt == 4 {
                    let prev = self.sfen_history.get(self.sfen_history.len() - 2).unwrap();

                    if cur.1 * 2 >= (i as u16) {
                        return Err(MoveError::PerpetualCheckLose);
                    } else if prev.1 * 2 >= (i as u16) {
                        return Err(MoveError::PerpetualCheckWin);
                    } else {
                        return Err(MoveError::Repetition);
                    }
                }
            }
        }

        Ok(())
    }

    /////////////////////////////////////////////////////////////////////////
    // SFEN serialization / deserialization
    /////////////////////////////////////////////////////////////////////////

    /// Parses the given SFEN string and updates the game state.
    pub fn set_sfen(&mut self, sfen_str: &str) -> Result<(), SfenError> {
        let mut parts = sfen_str.split_whitespace();

        // Build the initial position, all parts are required.
        parts
            .next()
            .ok_or(SfenError::MissingDataFields)
            .and_then(|s| self.parse_sfen_board(s))?;
        parts
            .next()
            .ok_or(SfenError::MissingDataFields)
            .and_then(|s| self.parse_sfen_stm(s))?;
        parts
            .next()
            .ok_or(SfenError::MissingDataFields)
            .and_then(|s| self.parse_sfen_hand(s))?;
        parts
            .next()
            .ok_or(SfenError::MissingDataFields)
            .and_then(|s| self.parse_sfen_ply(s))?;

        self.sfen_history.clear();
        self.log_position();

        // Make moves following the initial position, optional.
        if let Some("moves") = parts.next() {
            for m in parts {
                if let Some(m) = Move::from_sfen(m) {
                    // Stop if any error occurrs.
                    match self.make_move(m) {
                        Ok(_) => {
                            self.log_position();
                        }
                        Err(_) => break,
                    }
                } else {
                    return Err(SfenError::IllegalMove);
                }
            }
        }

        Ok(())
    }

    /// Converts the current state into SFEN formatted string.
    pub fn to_sfen(&self) -> String {
        if self.sfen_history.is_empty() {
            return self.generate_sfen();
        }

        if self.move_history.is_empty() {
            return format!("{} {}", self.sfen_history.first().unwrap().0, self.ply);
        }

        let mut sfen = format!(
            "{} {} moves",
            &self.sfen_history.first().unwrap().0,
            self.ply - self.move_history.len() as u16
        );

        for m in self.move_history.iter() {
            let _ = write!(sfen, " {}", &m.to_sfen());
        }

        sfen
    }

    fn parse_sfen_board(&mut self, s: &str) -> Result<(), SfenError> {
        let rows = s.split('/');

        self.occupied_bb = Bitboard::empty();
        self.color_bb = Default::default();
        self.type_bb = Default::default();

        for (i, row) in rows.enumerate() {
            if i >= 9 {
                return Err(SfenError::IllegalBoardState);
            }

            let mut j = 0;

            let mut is_promoted = false;
            for c in row.chars() {
                match c {
                    '+' => {
                        is_promoted = true;
                    }
                    n if n.is_ascii_digit() => {
                        if let Some(n) = n.to_digit(10) {
                            for _ in 0..n {
                                if j >= 9 {
                                    return Err(SfenError::IllegalBoardState);
                                }

                                let sq = Square::new(8 - j, i as u8).unwrap();
                                self.set_piece(sq, None);

                                j += 1;
                            }
                        }
                    }
                    s => match Piece::from_sfen(s) {
                        Some(mut piece) => {
                            if j >= 9 {
                                return Err(SfenError::IllegalBoardState);
                            }

                            if is_promoted {
                                if let Some(promoted) = piece.piece_type.promote() {
                                    piece.piece_type = promoted;
                                } else {
                                    return Err(SfenError::IllegalPieceType);
                                }
                            }

                            let sq = Square::new(8 - j, i as u8).unwrap();
                            self.set_piece(sq, Some(piece));
                            self.occupied_bb |= sq;
                            self.color_bb[piece.color.index()] |= sq;
                            self.type_bb[piece.piece_type.index()] |= sq;
                            j += 1;

                            is_promoted = false;
                        }
                        None => return Err(SfenError::IllegalPieceType),
                    },
                }
            }
        }

        Ok(())
    }

    fn parse_sfen_stm(&mut self, s: &str) -> Result<(), SfenError> {
        self.side_to_move = match s {
            "b" => Color::Black,
            "w" => Color::White,
            _ => return Err(SfenError::IllegalSideToMove),
        };
        Ok(())
    }

    fn parse_sfen_hand(&mut self, s: &str) -> Result<(), SfenError> {
        if s == "-" {
            self.hand.clear();
            return Ok(());
        }

        let mut num_pieces: u8 = 0;
        for c in s.chars() {
            match c {
                n if n.is_ascii_digit() => {
                    if let Some(n) = n.to_digit(10) {
                        num_pieces = num_pieces * 10 + (n as u8);
                    }
                }
                s => {
                    match Piece::from_sfen(s) {
                        Some(p) => self
                            .hand
                            .set(p, if num_pieces == 0 { 1 } else { num_pieces }),
                        None => return Err(SfenError::IllegalPieceType),
                    };
                    num_pieces = 0;
                }
            }
        }

        Ok(())
    }

    fn parse_sfen_ply(&mut self, s: &str) -> Result<(), SfenError> {
        self.ply = s.parse()?;
        Ok(())
    }

    fn generate_sfen(&self) -> String {
        let board = (0..9)
            .map(|row| {
                let mut s = String::new();
                let mut num_spaces = 0;
                for file in (0..9).rev() {
                    match *self.piece_at(Square::new(file, row).unwrap()) {
                        Some(pc) => {
                            if num_spaces > 0 {
                                s.push_str(&num_spaces.to_string());
                                num_spaces = 0;
                            }

                            s.push_str(&pc.to_string());
                        }
                        None => num_spaces += 1,
                    }
                }

                if num_spaces > 0 {
                    s.push_str(&num_spaces.to_string());
                }

                s
            })
            .join("/");

        let color = if self.side_to_move == Color::Black {
            "b"
        } else {
            "w"
        };

        let mut hand = [Color::Black, Color::White]
            .iter()
            .map(|c| {
                PieceType::iter()
                    .filter(|pt| pt.is_hand_piece())
                    .map(|pt| {
                        let pc = Piece {
                            piece_type: pt,
                            color: *c,
                        };
                        let n = self.hand.get(pc);

                        if n == 0 {
                            "".to_string()
                        } else if n == 1 {
                            format!("{pc}")
                        } else {
                            format!("{n}{pc}")
                        }
                    })
                    .join("")
            })
            .join("");

        if hand.is_empty() {
            hand = "-".to_string();
        }

        format!("{} {} {} {}", board, color, hand, self.ply)
    }
}

/////////////////////////////////////////////////////////////////////////////
// Trait implementations
/////////////////////////////////////////////////////////////////////////////

impl Default for Position {
    fn default() -> Position {
        Position {
            side_to_move: Color::Black,
            board: PieceGrid([None; 81]),
            hand: Default::default(),
            ply: 1,
            move_history: Default::default(),
            sfen_history: Default::default(),
            undone_moves: Default::default(),
            undone_sfen: Default::default(),
            occupied_bb: Default::default(),
            color_bb: Default::default(),
            type_bb: Default::default(),
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "   9   8   7   6   5   4   3   2   1")?;
        writeln!(f, "+---+---+---+---+---+---+---+---+---+")?;

        for row in 0..9 {
            write!(f, "|")?;
            for file in (0..9).rev() {
                if let Some(ref piece) = *self.piece_at(Square::new(file, row).unwrap()) {
                    write!(f, "{:>3}|", piece.to_string())?;
                } else {
                    write!(f, "   |")?;
                }
            }

            writeln!(f, " {}", (('a' as usize + row as usize) as u8) as char)?;
            writeln!(f, "+---+---+---+---+---+---+---+---+---+")?;
        }

        writeln!(
            f,
            "Side to move: {}",
            if self.side_to_move == Color::Black {
                "Black"
            } else {
                "White"
            }
        )?;

        let fmt_hand = |color: Color, f: &mut fmt::Formatter| -> fmt::Result {
            for pt in PieceType::iter().filter(|pt| pt.is_hand_piece()) {
                let pc = Piece {
                    piece_type: pt,
                    color,
                };
                let n = self.hand.get(pc);

                if n > 0 {
                    write!(f, "{pc}{n} ")?;
                }
            }
            Ok(())
        };
        write!(f, "Hand (Black): ")?;
        fmt_hand(Color::Black, f)?;
        writeln!(f)?;

        write!(f, "Hand (White): ")?;
        fmt_hand(Color::White, f)?;
        writeln!(f)?;

        write!(f, "Ply: {}", self.ply)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::square::consts::*;

    fn setup() {
        BBFactory::init();
    }

    #[test]
    fn new() {
        setup();

        let pos = Position::new();

        for i in 0..9 {
            for j in 0..9 {
                let sq = Square::new(i, j).unwrap();
                assert_eq!(None, *pos.piece_at(sq));
            }
        }
    }

    #[test]
    fn in_check() {
        setup();

        let test_cases = [
            (
                "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
                false,
                false,
            ),
            ("9/3r5/9/9/6B2/9/9/9/3K5 b P 1", true, false),
            (
                "ln2r1knl/2gb1+Rg2/4Pp1p1/p1pp1sp1p/1N2pN1P1/2P2PP2/PP1G1S2R/1SG6/LK6L w 2PSp 1",
                false,
                true,
            ),
            (
                "lnsg1gsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSG1GSNL b - 1",
                false,
                false,
            ),
        ];

        let mut pos = Position::new();
        for case in test_cases.iter() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert_eq!(case.1, pos.in_check(Color::Black));
            assert_eq!(case.2, pos.in_check(Color::White));
        }
    }

    #[test]
    fn find_king() {
        setup();

        let test_cases = [
            (
                "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
                Some(SQ_5I),
                Some(SQ_5A),
            ),
            ("9/3r5/9/9/6B2/9/9/9/3K5 b P 1", Some(SQ_6I), None),
            (
                "ln2r1knl/2gb1+Rg2/4Pp1p1/p1pp1sp1p/1N2pN1P1/2P2PP2/PP1G1S2R/1SG6/LK6L w 2PSp 1",
                Some(SQ_8I),
                Some(SQ_3A),
            ),
            (
                "lnsg1gsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSG1GSNL b - 1",
                None,
                None,
            ),
        ];

        let mut pos = Position::new();
        for case in test_cases.iter() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert_eq!(case.1, pos.find_king(Color::Black));
            assert_eq!(case.2, pos.find_king(Color::White));
        }
    }

    #[test]
    fn player_bb() {
        setup();

        let cases: &[(&str, &[Square], &[Square])] = &[
            (
                "R6gk/9/8p/9/4p4/9/9/8L/B8 b - 1",
                &[SQ_9A, SQ_1H, SQ_9I],
                &[SQ_2A, SQ_1A, SQ_1C, SQ_5E],
            ),
            ("9/3r5/9/9/6B2/9/9/9/3K5 b P 1", &[SQ_3E, SQ_6I], &[SQ_6B]),
        ];

        let mut pos = Position::new();
        for case in cases {
            pos.set_sfen(case.0).expect("faled to parse SFEN string");
            let black = pos.player_bb(Color::Black);
            let white = pos.player_bb(Color::White);

            assert_eq!(case.1.len(), black.count() as usize);
            for sq in case.1 {
                assert!((black & *sq).is_any());
            }

            assert_eq!(case.2.len(), white.count() as usize);
            for sq in case.2 {
                assert!((white & *sq).is_any());
            }
        }
    }

    #[test]
    fn pinned_bb() {
        setup();

        let cases: &[(&str, &[Square], &[Square])] = &[(
            "R6gk/9/8p/9/4p4/9/9/8L/B8 b - 1",
            &[],
            &[SQ_2A, SQ_1C, SQ_5E],
        )];

        let mut pos = Position::new();
        for case in cases {
            pos.set_sfen(case.0).expect("faled to parse SFEN string");
            let black = pos.pinned_bb(Color::Black);
            let white = pos.pinned_bb(Color::White);

            assert_eq!(case.1.len(), black.count());
            for sq in case.1 {
                assert!((&black & *sq).is_any());
            }

            assert_eq!(case.2.len(), white.count());
            for sq in case.2 {
                assert!((&white & *sq).is_any());
            }
        }
    }

    #[test]
    fn move_candidates() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        let mut sum = 0;
        for sq in Square::iter() {
            let pc = pos.piece_at(sq);

            if let Some(pc) = *pc {
                if pc.color == pos.side_to_move() {
                    sum += pos.move_candidates(sq, pc).count();
                }
            }
        }

        assert_eq!(30, sum);
    }

    #[test]
    fn make_normal_move() {
        setup();

        let base_sfen = "l6nl/5+P1gk/2np1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL w GR5pnsg 1";
        let test_cases = [
            (SQ_2B, SQ_2C, false, true),
            (SQ_7C, SQ_6E, false, true),
            (SQ_3I, SQ_4H, true, true),
            (SQ_6F, SQ_9I, true, true),
            (SQ_2B, SQ_2C, false, true),
            (SQ_9C, SQ_9D, false, false),
            (SQ_9B, SQ_8B, false, false),
            (SQ_9B, SQ_9D, false, false),
            (SQ_2B, SQ_2C, true, false),
        ];

        let mut pos = Position::new();
        for case in test_cases.iter() {
            pos.set_sfen(base_sfen)
                .expect("failed to parse SFEN string");
            assert_eq!(case.3, pos.make_normal_move(case.0, case.1, case.2).is_ok());
        }

        // Leaving the checked king is illegal.
        pos.set_sfen("9/3r5/9/9/6B2/9/9/9/3K5 b P 1")
            .expect("failed to parse SFEN string");
        assert!(pos.make_normal_move(SQ_6I, SQ_6H, false).is_err());
        pos.set_sfen("9/3r5/9/9/6B2/9/9/9/3K5 b P 1")
            .expect("failed to parse SFEN string");
        assert!(pos.make_normal_move(SQ_6I, SQ_7I, false).is_ok());
    }

    #[test]
    fn make_drop_move() {
        setup();

        let base_sfen = "l6nl/5+P1gk/2np1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL w GR5pnsg 1";
        let test_cases = [
            (SQ_5E, PieceType::Pawn, true),
            (SQ_5E, PieceType::Rook, false),
            (SQ_9A, PieceType::Pawn, false),
            (SQ_6F, PieceType::Pawn, false),
            (SQ_9B, PieceType::Pawn, false),
            (SQ_5I, PieceType::Pawn, false),
        ];

        let mut pos = Position::new();
        for case in test_cases.iter() {
            pos.set_sfen(base_sfen)
                .expect("failed to parse SFEN string");
            assert_eq!(
                case.2,
                pos.make_move(Move::Drop {
                    to: case.0,
                    piece_type: case.1,
                })
                .is_ok()
            );
        }
    }

    #[test]
    fn nifu() {
        setup();

        let ng_cases = [(
            "ln1g5/1ks1g3l/1p2p1n2/p1pGs2rp/1P1N1ppp1/P1SB1P2P/1S1p1bPP1/LKG6/4R2NL \
             w 2Pp 91",
            SQ_6C,
        )];
        let ok_cases = [(
            "ln1g5/1ks1g3l/1p2p1n2/p1pGs2rp/1P1N1ppp1/P1SB1P2P/1S1+p1bPP1/LKG6/4R2NL \
             w 2Pp 91",
            SQ_6C,
        )];

        let mut pos = Position::new();
        for (i, case) in ng_cases.iter().enumerate() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert_eq!(
                Some(MoveError::Nifu),
                pos.make_move(Move::Drop {
                    to: case.1,
                    piece_type: PieceType::Pawn,
                })
                .err(),
                "failed at #{i}"
            );
        }

        for (i, case) in ok_cases.iter().enumerate() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert!(
                pos.make_move(Move::Drop {
                    to: case.1,
                    piece_type: PieceType::Pawn,
                })
                .is_ok(),
                "failed at #{i}"
            );
        }
    }

    #[test]
    fn uchifuzume() {
        setup();

        let ng_cases = [
            ("9/9/7sp/6ppk/9/7G1/9/9/9 b P 1", SQ_1E),
            ("7nk/9/7S1/6b2/9/9/9/9/9 b P 1", SQ_1B),
            ("7nk/7g1/6BS1/9/9/9/9/9/9 b P 1", SQ_1B),
            ("R6gk/9/7S1/9/9/9/9/9/9 b P 1", SQ_1B),
        ];
        let ok_cases = [
            ("9/9/7pp/6psk/9/7G1/7N1/9/9 b P 1", SQ_1E),
            ("7nk/9/7Sg/6b2/9/9/9/9/9 b P 1", SQ_1B),
            (
                "9/8p/3pG1gp1/2p2kl1N/3P1p1s1/lPP6/2SGBP3/PK1S2+p2/LN7 w RSL3Prbg2n4p 1",
                SQ_8G,
            ),
            ("7k1/5G2l/6B2/9/9/9/9/9/9 b NP 1", SQ_2B),
        ];

        let mut pos = Position::new();
        for (i, case) in ng_cases.iter().enumerate() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert_eq!(
                Some(MoveError::Uchifuzume),
                pos.make_move(Move::Drop {
                    to: case.1,
                    piece_type: PieceType::Pawn,
                })
                .err(),
                "failed at #{i}"
            );
        }

        for (i, case) in ok_cases.iter().enumerate() {
            pos.set_sfen(case.0).expect("failed to parse SFEN string");
            assert!(
                pos.make_move(Move::Drop {
                    to: case.1,
                    piece_type: PieceType::Pawn,
                })
                .is_ok(),
                "failed at #{i}"
            );
        }
    }

    #[test]
    fn repetition() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("ln7/ks+R6/pp7/9/9/9/9/9/9 b Ss 1")
            .expect("failed to parse SFEN string");

        for _ in 0..2 {
            assert!(pos.make_drop_move(SQ_7A, PieceType::Silver).is_ok());
            assert!(pos.make_drop_move(SQ_7C, PieceType::Silver).is_ok());
            assert!(pos.make_normal_move(SQ_7A, SQ_8B, true).is_ok());
            assert!(pos.make_normal_move(SQ_7C, SQ_8B, false).is_ok());
        }

        assert!(pos.make_drop_move(SQ_7A, PieceType::Silver).is_ok());
        assert!(pos.make_drop_move(SQ_7C, PieceType::Silver).is_ok());
        assert!(pos.make_normal_move(SQ_7A, SQ_8B, true).is_ok());
        assert_eq!(
            Some(MoveError::Repetition),
            pos.make_normal_move(SQ_7C, SQ_8B, false).err()
        );
    }

    #[test]
    fn percetual_check() {
        setup();

        // Case 1. Starting from a check move.
        let mut pos = Position::new();
        pos.set_sfen("8l/6+P2/6+Rpk/8p/9/7S1/9/9/9 b - 1")
            .expect("failed to parse SFEN string");

        for _ in 0..2 {
            assert!(pos.make_normal_move(SQ_3C, SQ_2B, false).is_ok());
            assert!(pos.make_normal_move(SQ_1C, SQ_2D, false).is_ok());
            assert!(pos.make_normal_move(SQ_2B, SQ_3C, false).is_ok());
            assert!(pos.make_normal_move(SQ_2D, SQ_1C, false).is_ok());
        }
        assert!(pos.make_normal_move(SQ_3C, SQ_2B, false).is_ok());
        assert!(pos.make_normal_move(SQ_1C, SQ_2D, false).is_ok());
        assert!(pos.make_normal_move(SQ_2B, SQ_3C, false).is_ok());
        assert_eq!(
            Some(MoveError::PerpetualCheckWin),
            pos.make_normal_move(SQ_2D, SQ_1C, false).err()
        );

        // Case 2. Starting from an escape move.
        pos.set_sfen("6p1k/9/8+R/9/9/9/9/9/9 w - 1")
            .expect("failed to parse SFEN string");

        for _ in 0..2 {
            assert!(pos.make_normal_move(SQ_1A, SQ_2A, false).is_ok());
            assert!(pos.make_normal_move(SQ_1C, SQ_2C, false).is_ok());
            assert!(pos.make_normal_move(SQ_2A, SQ_1A, false).is_ok());
            assert!(pos.make_normal_move(SQ_2C, SQ_1C, false).is_ok());
        }
        assert!(pos.make_normal_move(SQ_1A, SQ_2A, false).is_ok());
        assert!(pos.make_normal_move(SQ_1C, SQ_2C, false).is_ok());
        assert!(pos.make_normal_move(SQ_2A, SQ_1A, false).is_ok());
        assert_eq!(
            Some(MoveError::PerpetualCheckLose),
            pos.make_normal_move(SQ_2C, SQ_1C, false).err()
        );
    }

    #[test]
    fn unmake_move() {
        setup();

        let mut pos = Position::new();
        let base_sfen = "l6nl/4+p+P1gk/2n2S3/p1p4Pp/3P2Sp1/1PPb2P1P/4+P1GS1/R8/LN4bKL w RG5gsnp 1";
        pos.set_sfen(base_sfen)
            .expect("failed to parse SFEN string");
        let base_state = format!("{pos}");
        println!("{base_state}");
        let test_cases = [
            Move::Drop {
                to: SQ_5E,
                piece_type: PieceType::Pawn,
            },
            // No capture by unpromoted piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_7G,
                promote: false,
            },
            // No capture by promoting piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_7G,
                promote: true,
            },
            // No capture by promoted piece
            Move::Normal {
                from: SQ_5B,
                to: SQ_5A,
                promote: false,
            },
            // Capture of unpromoted piece by unpromoted piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_9I,
                promote: false,
            },
            // Capture of unpromoted piece by promoting piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_9I,
                promote: true,
            },
            // Capture of unpromoted piece by promoted piece
            Move::Normal {
                from: SQ_5B,
                to: SQ_4C,
                promote: false,
            },
            // Capture of promoted piece by unpromoted piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_5G,
                promote: false,
            },
            // Capture of promoted piece by promoting piece
            Move::Normal {
                from: SQ_6F,
                to: SQ_5G,
                promote: true,
            },
            // Capture of promoted piece by promoted piece
            Move::Normal {
                from: SQ_5B,
                to: SQ_4B,
                promote: false,
            },
        ];

        for case in test_cases.iter() {
            pos.set_sfen(base_sfen)
                .expect("failed to parse SFEN string");
            pos.make_move(*case)
                .unwrap_or_else(|_| panic!("failed to make a move: {case}"));
            pos.unmake_move()
                .unwrap_or_else(|_| panic!("failed to unmake a move: {case}"));
            assert_eq!(
                base_sfen,
                pos.to_sfen(),
                "{}",
                format!("sfen unmatch for {case}").as_str()
            );
            assert_eq!(
                base_state,
                format!("{pos}"),
                "{}",
                format!("state unmatch for {case}").as_str()
            );
        }
    }

    #[test]
    fn try_declare_winning() {
        setup();

        let mut pos = Position::new();

        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen("1K7/+NG+N+NGG3/P+S+P+P+PS3/9/7s1/9/+b+rppp+p+s1+p/3+p1+bk2/9 b R4L7Pgnp 1")
            .expect("failed to parse SFEN string");
        assert!(pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen(
            "1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/3+b4P/4+p+p+bs1/+r1s4+lk/1g1g3+r1 w \
             Gns2l11p 1",
        )
        .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(pos.try_declare_winning(Color::White));

        pos.set_sfen(
            "1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/3+b4P/4+p+p+bs1/+r1s4+lk/1g1g3+r1 b \
             Gns2l11p 1",
        )
        .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen(
            "1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/3+b4P/4+p+p+bs1/+r1s4+l1/1g1g3+r1 b \
             Gns2l11p 1",
        )
        .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen(
            "1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/1k1+b4P/4+p+p+bs1/+r1s4+l1/1g1g3+r1 b \
             Gns2l11p 1",
        )
        .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen(
            "1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/3+b4P/4+p+p+bs1/+r1s4+lk/1g1g3+rG w \
             ns2l11p 1",
        )
        .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));

        pos.set_sfen("1K6l/1+N7/+PG2+Ns1p1/2+N5p/6p2/3+b4P/5+p+bs1/+r1s4+lk/1g1g3+rG w ns2l12p 1")
            .expect("failed to parse SFEN string");
        assert!(!pos.try_declare_winning(Color::Black));
        assert!(!pos.try_declare_winning(Color::White));
    }

    #[test]
    fn set_sfen_normal() {
        setup();

        let mut pos = Position::new();

        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        let filled_squares = [
            (0, 0, PieceType::Lance, Color::White),
            (1, 0, PieceType::Knight, Color::White),
            (2, 0, PieceType::Silver, Color::White),
            (3, 0, PieceType::Gold, Color::White),
            (4, 0, PieceType::King, Color::White),
            (5, 0, PieceType::Gold, Color::White),
            (6, 0, PieceType::Silver, Color::White),
            (7, 0, PieceType::Knight, Color::White),
            (8, 0, PieceType::Lance, Color::White),
            (7, 1, PieceType::Rook, Color::White),
            (1, 1, PieceType::Bishop, Color::White),
            (0, 2, PieceType::Pawn, Color::White),
            (1, 2, PieceType::Pawn, Color::White),
            (2, 2, PieceType::Pawn, Color::White),
            (3, 2, PieceType::Pawn, Color::White),
            (4, 2, PieceType::Pawn, Color::White),
            (5, 2, PieceType::Pawn, Color::White),
            (6, 2, PieceType::Pawn, Color::White),
            (7, 2, PieceType::Pawn, Color::White),
            (8, 2, PieceType::Pawn, Color::White),
            (0, 6, PieceType::Pawn, Color::Black),
            (1, 6, PieceType::Pawn, Color::Black),
            (2, 6, PieceType::Pawn, Color::Black),
            (3, 6, PieceType::Pawn, Color::Black),
            (4, 6, PieceType::Pawn, Color::Black),
            (5, 6, PieceType::Pawn, Color::Black),
            (6, 6, PieceType::Pawn, Color::Black),
            (7, 6, PieceType::Pawn, Color::Black),
            (8, 6, PieceType::Pawn, Color::Black),
            (7, 7, PieceType::Bishop, Color::Black),
            (1, 7, PieceType::Rook, Color::Black),
            (0, 8, PieceType::Lance, Color::Black),
            (1, 8, PieceType::Knight, Color::Black),
            (2, 8, PieceType::Silver, Color::Black),
            (3, 8, PieceType::Gold, Color::Black),
            (4, 8, PieceType::King, Color::Black),
            (5, 8, PieceType::Gold, Color::Black),
            (6, 8, PieceType::Silver, Color::Black),
            (7, 8, PieceType::Knight, Color::Black),
            (8, 8, PieceType::Lance, Color::Black),
        ];

        let empty_squares = [
            (0, 1, 1),
            (2, 1, 5),
            (8, 1, 1),
            (0, 3, 9),
            (0, 4, 9),
            (0, 5, 9),
            (0, 7, 1),
            (2, 7, 5),
            (8, 7, 1),
        ];

        let hand_pieces = [
            (PieceType::Pawn, 0),
            (PieceType::Lance, 0),
            (PieceType::Knight, 0),
            (PieceType::Silver, 0),
            (PieceType::Gold, 0),
            (PieceType::Rook, 0),
            (PieceType::Bishop, 0),
        ];

        for case in filled_squares.iter() {
            let (file, row, pt, c) = *case;
            assert_eq!(
                Some(Piece {
                    piece_type: pt,
                    color: c,
                }),
                *pos.piece_at(Square::new(file, row).unwrap())
            );
        }

        for case in empty_squares.iter() {
            let (file, row, len) = *case;
            for i in file..(file + len) {
                assert_eq!(None, *pos.piece_at(Square::new(i, row).unwrap()));
            }
        }

        for case in hand_pieces.iter() {
            let (pt, n) = *case;
            assert_eq!(
                n,
                pos.hand(Piece {
                    piece_type: pt,
                    color: Color::Black,
                })
            );
            assert_eq!(
                n,
                pos.hand(Piece {
                    piece_type: pt,
                    color: Color::White,
                })
            );
        }

        assert_eq!(Color::Black, pos.side_to_move());
        assert_eq!(1, pos.ply());
    }

    #[test]
    fn to_sfen() {
        setup();

        let test_cases = [
            "7k1/9/7P1/9/9/9/9/9/9 b G2r2b3g4s4n4l17p 1",
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
            "lnsgk+Lpnl/1p5+B1/p1+Pps1ppp/9/9/9/P+r1PPpPPP/1R7/LNSGKGSN1 w BGP2p \
             1024",
        ];

        let mut pos = Position::new();
        for case in test_cases.iter() {
            pos.set_sfen(case).expect("failed to parse SFEN string");
            assert_eq!(*case, pos.to_sfen());
        }
    }

    #[test]
    fn set_sfen_custom() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgk+Lpnl/1p5+B1/p1+Pps1ppp/9/9/9/P+r1PPpPPP/1R7/LNSGKGSN1 w BGP2p 1024")
            .expect("failed to parse SFEN string");

        let filled_squares = [
            (8, 0, PieceType::Lance, Color::White),
            (7, 0, PieceType::Knight, Color::White),
            (6, 0, PieceType::Silver, Color::White),
            (5, 0, PieceType::Gold, Color::White),
            (4, 0, PieceType::King, Color::White),
            (3, 0, PieceType::ProLance, Color::Black),
            (2, 0, PieceType::Pawn, Color::White),
            (1, 0, PieceType::Knight, Color::White),
            (0, 0, PieceType::Lance, Color::White),
            (7, 1, PieceType::Pawn, Color::White),
            (1, 1, PieceType::ProBishop, Color::Black),
            (8, 2, PieceType::Pawn, Color::White),
            (6, 2, PieceType::ProPawn, Color::Black),
            (5, 2, PieceType::Pawn, Color::White),
            (4, 2, PieceType::Silver, Color::White),
            (2, 2, PieceType::Pawn, Color::White),
            (1, 2, PieceType::Pawn, Color::White),
            (0, 2, PieceType::Pawn, Color::White),
            (8, 6, PieceType::Pawn, Color::Black),
            (7, 6, PieceType::ProRook, Color::White),
            (5, 6, PieceType::Pawn, Color::Black),
            (4, 6, PieceType::Pawn, Color::Black),
            (3, 6, PieceType::Pawn, Color::White),
            (2, 6, PieceType::Pawn, Color::Black),
            (1, 6, PieceType::Pawn, Color::Black),
            (0, 6, PieceType::Pawn, Color::Black),
            (7, 7, PieceType::Rook, Color::Black),
            (8, 8, PieceType::Lance, Color::Black),
            (7, 8, PieceType::Knight, Color::Black),
            (6, 8, PieceType::Silver, Color::Black),
            (5, 8, PieceType::Gold, Color::Black),
            (4, 8, PieceType::King, Color::Black),
            (3, 8, PieceType::Gold, Color::Black),
            (2, 8, PieceType::Silver, Color::Black),
            (1, 8, PieceType::Knight, Color::Black),
        ];

        let empty_squares = [
            (0, 1, 1),
            (2, 1, 5),
            (8, 1, 1),
            (3, 2, 1),
            (7, 2, 1),
            (0, 3, 9),
            (0, 4, 9),
            (0, 5, 9),
            (6, 6, 1),
            (0, 7, 7),
            (8, 7, 1),
            (0, 8, 1),
        ];

        let hand_pieces = [
            (
                Piece {
                    piece_type: PieceType::Pawn,
                    color: Color::Black,
                },
                1,
            ),
            (
                Piece {
                    piece_type: PieceType::Gold,
                    color: Color::Black,
                },
                1,
            ),
            (
                Piece {
                    piece_type: PieceType::Bishop,
                    color: Color::Black,
                },
                1,
            ),
            (
                Piece {
                    piece_type: PieceType::Pawn,
                    color: Color::White,
                },
                2,
            ),
        ];

        for case in filled_squares.iter() {
            let (file, row, pt, c) = *case;
            assert_eq!(
                Some(Piece {
                    piece_type: pt,
                    color: c,
                }),
                *pos.piece_at(Square::new(file, row).unwrap())
            );
        }

        for case in empty_squares.iter() {
            let (file, row, len) = *case;
            for i in file..(file + len) {
                assert_eq!(None, *pos.piece_at(Square::new(i, row).unwrap()));
            }
        }

        for case in hand_pieces.iter() {
            let (p, n) = *case;
            assert_eq!(n, pos.hand(p));
        }

        assert_eq!(Color::White, pos.side_to_move());
        assert_eq!(1024, pos.ply());
    }

    #[test]
    fn is_attacked_by() {
        setup();

        let mut pos = Position::new();
        // Starting position
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Black's king on 5i is not attacked by white
        assert!(!pos.is_attacked_by(SQ_5I, Color::White));
        // White's king on 5a is not attacked by black
        assert!(!pos.is_attacked_by(SQ_5A, Color::Black));

        // Squares in front of black pawns are "attacked" by black pawns
        assert!(pos.is_attacked_by(SQ_1F, Color::Black));
        assert!(pos.is_attacked_by(SQ_5F, Color::Black));

        // Squares in front of white pawns are "attacked" by white pawns
        assert!(pos.is_attacked_by(SQ_1D, Color::White));
        assert!(pos.is_attacked_by(SQ_5D, Color::White));

        // Position with a rook attacking squares
        pos.set_sfen("9/3r5/9/9/6B2/9/9/9/3K5 b P 1")
            .expect("failed to parse SFEN string");

        // The rook on 6b attacks along its file and rank
        assert!(pos.is_attacked_by(SQ_6I, Color::White)); // Down the file
        assert!(pos.is_attacked_by(SQ_6A, Color::White)); // Up the file
        assert!(pos.is_attacked_by(SQ_1B, Color::White)); // Along the rank
        assert!(pos.is_attacked_by(SQ_9B, Color::White)); // Along the rank
    }

    #[test]
    fn attackers() {
        setup();

        let mut pos = Position::new();
        // Position with multiple attackers on a square
        // Knights on 5g and 3g, rook on 5d
        pos.set_sfen("9/9/9/4r4/9/9/2N1N4/4K4/9 b - 1")
            .expect("failed to parse SFEN string");

        // The square 5f is attacked by white rook from 5d
        let attackers_on_5f = pos.attackers(SQ_5F, Color::White);
        assert_eq!(1, attackers_on_5f.count());
        assert!(attackers_on_5f.contains(SQ_5D));

        // Shogi knights move 2 forward and 1 to the side
        // Knight on 5g attacks 4e and 6e
        // Knight on 7g attacks 6e and 8e
        // So 6e is attacked by both knights
        pos.set_sfen("9/9/9/9/9/9/2N1N4/4K4/9 b - 1")
            .expect("failed to parse SFEN string");

        let attackers_on_6e = pos.attackers(SQ_6E, Color::Black);
        assert_eq!(2, attackers_on_6e.count());
        assert!(attackers_on_6e.contains(SQ_5G));
        assert!(attackers_on_6e.contains(SQ_7G));

        // Empty square with no attackers
        let attackers_on_9i = pos.attackers(SQ_9I, Color::Black);
        assert_eq!(0, attackers_on_9i.count());
    }

    #[test]
    fn has_legal_moves_starting_position() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Both players have legal moves in starting position
        assert!(pos.has_legal_moves(Color::Black));
        assert!(pos.has_legal_moves(Color::White));

        // Neither player is in checkmate or stalemate
        assert!(!pos.is_checkmate(Color::Black));
        assert!(!pos.is_checkmate(Color::White));
        assert!(!pos.is_stalemate(Color::Black));
        assert!(!pos.is_stalemate(Color::White));
    }

    #[test]
    fn checkmate_detection() {
        setup();

        let mut pos = Position::new();

        // Simple checkmate position: White king on 1a, Black rook on 1b giving check,
        // Black gold on 2b preventing escape
        // The king has no escape squares and the checking piece cannot be captured or blocked
        pos.set_sfen("k8/RG7/9/9/9/9/9/9/8K b - 1")
            .expect("failed to parse SFEN string");

        // White (king on 1a) is in checkmate
        assert!(pos.in_check(Color::White));
        assert!(!pos.has_legal_moves(Color::White));
        assert!(pos.is_checkmate(Color::White));
        assert!(!pos.is_stalemate(Color::White));

        // Black is not in check or checkmate
        assert!(!pos.in_check(Color::Black));
        assert!(pos.has_legal_moves(Color::Black));
        assert!(!pos.is_checkmate(Color::Black));
    }

    #[test]
    fn not_checkmate_when_can_escape() {
        setup();

        let mut pos = Position::new();

        // King in check but can escape
        // White king on 5a, Black rook on 5i giving check
        // King can escape to 4a, 6a, etc.
        pos.set_sfen("4k4/9/9/9/9/9/9/9/4R3K b - 1")
            .expect("failed to parse SFEN string");

        // White king is in check but not checkmate (can move sideways)
        assert!(pos.in_check(Color::White));
        assert!(pos.has_legal_moves(Color::White));
        assert!(!pos.is_checkmate(Color::White));
    }

    #[test]
    fn not_checkmate_when_can_block() {
        setup();

        let mut pos = Position::new();

        // King in check but can be blocked
        // White king on 1a with a gold on 9a that can block the rook's attack
        pos.set_sfen("k7g/9/9/9/9/9/9/9/R7K b - 1")
            .expect("failed to parse SFEN string");

        // White king is in check but gold can block
        assert!(pos.in_check(Color::White));
        assert!(pos.has_legal_moves(Color::White));
        assert!(!pos.is_checkmate(Color::White));
    }

    #[test]
    fn not_checkmate_when_can_capture() {
        setup();

        let mut pos = Position::new();

        // King in check but can capture the checking piece
        // White king on 1a, Black rook on 2a (can be captured by king)
        pos.set_sfen("kR7/9/9/9/9/9/9/9/8K b - 1")
            .expect("failed to parse SFEN string");

        // White king is in check but can capture the rook
        assert!(pos.in_check(Color::White));
        assert!(pos.has_legal_moves(Color::White));
        assert!(!pos.is_checkmate(Color::White));
    }

    #[test]
    fn is_legal_move_test() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Legal move: pawn from 7g to 7f
        let legal_move = Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        assert!(pos.is_legal_move(&legal_move));

        // Illegal move: moving a piece to an occupied square of same color
        let illegal_move = Move::Normal {
            from: SQ_9I,
            to: SQ_9G,
            promote: false,
        };
        assert!(!pos.is_legal_move(&illegal_move));

        // Note: is_legal_move infers the color from the piece being moved,
        // so it checks if the move would be legal for that piece's color.
        // Use is_legal_move_for to explicitly check for a specific color.
        let white_move = Move::Normal {
            from: SQ_7C,
            to: SQ_7D,
            promote: false,
        };
        // This is legal for White (the piece's color)
        assert!(pos.is_legal_move(&white_move));
        // But illegal if we explicitly check for Black
        assert!(!pos.is_legal_move_for(&white_move, Color::Black));
    }

    #[test]
    fn impasse_points_calculation() {
        setup();

        let mut pos = Position::new();

        // Position with black king in promotion zone, some pieces there too
        // 1K7/+NG+N+NGG3/P+S+P+P+PS3/9/9/9/+b+rppp+p+s1+p/3+p1+bk2/9 b R4L7Pgnp 1
        // Black king is at 8a (promotion zone), with several pieces in zone
        pos.set_sfen("1K7/+NG+N+NGG3/P+S+P+P+PS3/9/9/9/+b+rppp+p+s1+p/3+p1+bk2/9 b R4L7Pgnp 1")
            .expect("failed to parse SFEN string");

        // Black has pieces in promotion zone + hand pieces
        let black_points = pos.impasse_points(Color::Black);
        let black_pieces = pos.pieces_in_promotion_zone(Color::Black);

        // Verify black has enough pieces and points for declaration
        assert!(black_pieces >= 10, "Black should have 10+ pieces in zone");
        assert!(black_points >= 28, "Black should have 28+ points");

        // Verify try_declare_winning agrees with our calculation
        assert!(pos.try_declare_winning(Color::Black));
    }

    #[test]
    fn impasse_not_triggered() {
        setup();

        let mut pos = Position::new();

        // Starting position - not an impasse
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        assert_eq!(ImpasseResult::None, pos.check_impasse());
    }

    #[test]
    fn pieces_in_promotion_zone_count() {
        setup();

        let mut pos = Position::new();

        // Starting position - pawns are not in promotion zone
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // No pieces in promotion zone at start
        assert_eq!(0, pos.pieces_in_promotion_zone(Color::Black));
        assert_eq!(0, pos.pieces_in_promotion_zone(Color::White));

        // Position with black king and some pieces in promotion zone
        pos.set_sfen("1K7/+NG+N+NGG3/P+S+P+P+PS3/9/9/9/+b+rppp+p+s1+p/3+p1+bk2/9 b R4L7Pgnp 1")
            .expect("failed to parse SFEN string");

        // Black has pieces in ranks a, b, c (the promotion zone for black)
        let black_count = pos.pieces_in_promotion_zone(Color::Black);
        assert!(black_count >= 10, "Black should have 10+ pieces in promotion zone");
    }

    #[test]
    fn undo_preserves_history() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Make some moves
        pos.make_move(Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        })
        .unwrap();
        pos.make_move(Move::Normal {
            from: SQ_3C,
            to: SQ_3D,
            promote: false,
        })
        .unwrap();
        pos.make_move(Move::Normal {
            from: SQ_2G,
            to: SQ_2F,
            promote: false,
        })
        .unwrap();

        assert_eq!(3, pos.move_history().len());
        assert_eq!(0, pos.undone_moves().len());

        // Undo one move
        pos.unmake_move().unwrap();
        assert_eq!(2, pos.move_history().len());
        assert_eq!(1, pos.undone_moves().len());

        // Full history should still have all 3 moves
        let full = pos.full_history();
        assert_eq!(3, full.len());

        // Undo another move
        pos.unmake_move().unwrap();
        assert_eq!(1, pos.move_history().len());
        assert_eq!(2, pos.undone_moves().len());

        // Full history should still have all 3 moves
        let full = pos.full_history();
        assert_eq!(3, full.len());
    }

    #[test]
    fn redo_move_works() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Make a move
        let original_move = Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        pos.make_move(original_move).unwrap();

        // Undo it
        pos.unmake_move().unwrap();
        assert!(pos.can_redo());
        assert_eq!(1, pos.can_redo_count());

        // Redo it
        let redone = pos.redo_move().unwrap();
        assert!(!pos.can_redo());
        assert_eq!(0, pos.can_redo_count());

        // The redone move should match
        assert_eq!(redone, original_move);

        // Position should be the same as after the original move
        assert_eq!(1, pos.move_history().len());
    }

    #[test]
    fn branching_clears_undo_stack() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        // Make two moves
        pos.make_move(Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        })
        .unwrap();
        pos.make_move(Move::Normal {
            from: SQ_3C,
            to: SQ_3D,
            promote: false,
        })
        .unwrap();

        // Undo both
        pos.unmake_move().unwrap();
        pos.unmake_move().unwrap();
        assert_eq!(2, pos.undone_moves().len());

        // Make a different move (branching)
        pos.make_move(Move::Normal {
            from: SQ_2G,
            to: SQ_2F,
            promote: false,
        })
        .unwrap();

        // Undone moves should be cleared
        assert_eq!(0, pos.undone_moves().len());
        assert!(!pos.can_redo());
    }

    #[test]
    fn legal_moves_starting_position() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .expect("failed to parse SFEN string");

        let moves = pos.legal_moves();

        // In the starting position, Black has 30 legal moves
        // (9 pawn moves + various piece moves)
        assert_eq!(30, moves.len());

        // All returned moves should be valid
        for mv in &moves {
            assert!(pos.is_legal_move(mv));
        }
    }

    #[test]
    fn legal_moves_with_drops() {
        setup();

        let mut pos = Position::new();
        // Position with pieces in hand
        pos.set_sfen("4k4/9/9/9/9/9/9/9/4K4 b P 1")
            .expect("failed to parse SFEN string");

        let moves = pos.legal_moves();

        // Should have some drop moves (pawn in hand)
        let drop_moves: Vec<_> = moves
            .iter()
            .filter(|m| matches!(m, Move::Drop { .. }))
            .collect();

        // Can drop pawn on most empty squares (excluding rank 1 which would be immovable)
        assert!(!drop_moves.is_empty());

        // All returned moves should be valid
        for mv in &moves {
            assert!(pos.is_legal_move(mv));
        }
    }

    #[test]
    fn legal_moves_in_check() {
        setup();

        let mut pos = Position::new();
        // Position where black king is in check
        pos.set_sfen("4k4/9/9/9/9/4r4/9/9/4K4 b - 1")
            .expect("failed to parse SFEN string");

        // Black is in check
        assert!(pos.in_check(Color::Black));

        let moves = pos.legal_moves();

        // All legal moves must escape check
        for mv in &moves {
            let mut test_pos = pos.clone();
            test_pos.make_move(*mv).unwrap();
            assert!(!test_pos.in_check(Color::Black));
        }
    }

    #[test]
    fn legal_moves_consistency_with_has_legal_moves() {
        setup();

        let mut pos = Position::new();

        // Various positions to test
        let test_positions = [
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
            "4k4/9/9/9/9/9/9/9/4K4 b P 1",
            "k8/RG7/9/9/9/9/9/9/8K b - 1", // Checkmate position for white
        ];

        for sfen in test_positions {
            pos.set_sfen(sfen).expect("failed to parse SFEN string");

            let has_moves = pos.has_legal_moves(Color::Black);
            let moves = pos.legal_moves_for(Color::Black);

            assert_eq!(has_moves, !moves.is_empty());
        }
    }

    #[test]
    fn hosking_notation() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .unwrap();

        // Pawn to 7f (file 2, rank 5 in 0-indexed = file 7, rank 6 in Hosking)
        let mv = Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        pos.make_move(mv).unwrap();

        let record = &pos.move_history()[0];
        assert_eq!("P76", record.to_hosking());
    }

    #[test]
    fn hodges_notation() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .unwrap();

        // Pawn to 7f
        let mv = Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        pos.make_move(mv).unwrap();

        let record = &pos.move_history()[0];
        assert_eq!("P7f", record.to_hodges());
    }

    #[test]
    fn drop_notation() {
        setup();

        let mut pos = Position::new();
        // Position with gold in hand
        pos.set_sfen("4k4/9/9/9/9/9/9/9/4K4 b G 1").unwrap();

        // Drop gold at 5e (file 4, rank 4 in 0-indexed)
        let mv = Move::Drop {
            to: SQ_5E,
            piece_type: PieceType::Gold,
        };
        pos.make_move(mv).unwrap();

        let record = &pos.move_history()[0];
        assert_eq!("G*55", record.to_hosking());
        assert_eq!("G*5e", record.to_hodges());
    }

    #[test]
    fn promotion_notation() {
        setup();

        let mut pos = Position::new();
        // Position with silver about to promote
        pos.set_sfen("4k4/9/4S4/9/9/9/9/9/4K4 b - 1").unwrap();

        // Silver promotes at 5b (file 4, rank 1 in 0-indexed)
        let mv = Move::Normal {
            from: SQ_5C,
            to: SQ_5B,
            promote: true,
        };
        pos.make_move(mv).unwrap();

        let record = &pos.move_history()[0];
        assert_eq!("S52+", record.to_hosking());
        assert_eq!("S5b+", record.to_hodges());
    }

    #[test]
    fn to_notation_format() {
        setup();

        let mut pos = Position::new();
        pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .unwrap();

        let mv = Move::Normal {
            from: SQ_7G,
            to: SQ_7F,
            promote: false,
        };
        pos.make_move(mv).unwrap();

        let record = &pos.move_history()[0];

        // Test all notation formats via to_notation()
        assert_eq!("☗７六歩", record.to_notation(NotationFormat::Japanese));
        assert_eq!("P76", record.to_notation(NotationFormat::Hosking));
        assert_eq!("P7f", record.to_notation(NotationFormat::Hodges));
    }
}
