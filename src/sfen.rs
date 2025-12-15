//! SFEN (Shogi Forsyth-Edwards Notation) utilities
//!
//! This module provides types and functions for working with SFEN strings.
//! SFEN is the standard notation for representing Shogi positions.
//!
//! # Types
//!
//! - [`Sfen`]: A parsed SFEN string with its components
//!
//! # Utility Functions
//!
//! - [`mirror_sfen`]: Rotate a position 180 degrees
//! - [`reset_move_number`]: Reset the move counter to 1
//! - [`swap_piece_colors`]: Swap uppercase/lowercase in a string
//! - [`consolidate_sfen_row`]: Merge adjacent empty square counts
//! - [`remove_piece_from_sfen`]: Remove a piece from a board string

use crate::color::Color;
use crate::error::SfenError;
use std::fmt;
use std::str::FromStr;

/// A parsed SFEN (Shogi Forsyth-Edwards Notation) string.
///
/// This struct represents the components of a SFEN string and provides
/// idiomatic Rust parsing and formatting via [`FromStr`] and [`Display`].
///
/// # Examples
///
/// ```
/// use shogi::sfen::Sfen;
/// use shogi::Color;
///
/// // Parse a SFEN string
/// let sfen: Sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
///     .parse()
///     .unwrap();
///
/// assert_eq!(sfen.side_to_move(), Color::Black);
/// assert_eq!(sfen.ply(), 1);
///
/// // Convert back to string
/// assert_eq!(
///     sfen.to_string(),
///     "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
/// );
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sfen {
    board: String,
    side_to_move: Color,
    hand: String,
    ply: u16,
    moves: Vec<String>,
}

impl Sfen {
    /// Creates a new Sfen from its components.
    pub fn new(board: String, side_to_move: Color, hand: String, ply: u16) -> Self {
        Self {
            board,
            side_to_move,
            hand,
            ply,
            moves: Vec::new(),
        }
    }

    /// Creates a new Sfen with a move list.
    pub fn with_moves(
        board: String,
        side_to_move: Color,
        hand: String,
        ply: u16,
        moves: Vec<String>,
    ) -> Self {
        Self {
            board,
            side_to_move,
            hand,
            ply,
            moves,
        }
    }

    /// Returns the board portion of the SFEN.
    pub fn board(&self) -> &str {
        &self.board
    }

    /// Returns the side to move.
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Returns the hand pieces portion of the SFEN.
    pub fn hand(&self) -> &str {
        &self.hand
    }

    /// Returns the ply (move number).
    pub fn ply(&self) -> u16 {
        self.ply
    }

    /// Returns the move list, if any.
    pub fn moves(&self) -> &[String] {
        &self.moves
    }

    /// Returns whether this SFEN includes a move list.
    pub fn has_moves(&self) -> bool {
        !self.moves.is_empty()
    }

    /// Returns the starting position SFEN for standard shogi.
    pub fn startpos() -> Self {
        Self {
            board: "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL".to_string(),
            side_to_move: Color::Black,
            hand: "-".to_string(),
            ply: 1,
            moves: Vec::new(),
        }
    }
}

impl FromStr for Sfen {
    type Err = SfenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split_whitespace();

        let board = parts
            .next()
            .ok_or(SfenError::MissingDataFields)?
            .to_string();

        let side_to_move = match parts.next().ok_or(SfenError::MissingDataFields)? {
            "b" => Color::Black,
            "w" => Color::White,
            _ => return Err(SfenError::IllegalSideToMove),
        };

        let hand = parts
            .next()
            .ok_or(SfenError::MissingDataFields)?
            .to_string();

        let ply: u16 = parts
            .next()
            .ok_or(SfenError::MissingDataFields)?
            .parse()?;

        // Parse optional moves section
        let moves = if let Some("moves") = parts.next() {
            parts.map(String::from).collect()
        } else {
            Vec::new()
        };

        Ok(Self {
            board,
            side_to_move,
            hand,
            ply,
            moves,
        })
    }
}

impl fmt::Display for Sfen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stm = if self.side_to_move == Color::Black {
            "b"
        } else {
            "w"
        };

        if self.moves.is_empty() {
            write!(f, "{} {} {} {}", self.board, stm, self.hand, self.ply)
        } else {
            write!(
                f,
                "{} {} {} {} moves {}",
                self.board,
                stm,
                self.hand,
                self.ply,
                self.moves.join(" ")
            )
        }
    }
}

/// Mirror/rotate an SFEN 180 degrees to swap player perspectives
///
/// This performs a full board rotation:
/// - Flips piece positions (both horizontally and vertically)
/// - Swaps piece colors (uppercase ↔ lowercase)
/// - Swaps side-to-move indicator (if present)
/// - Swaps hand piece colors (if present)
///
/// Works with both full SFEN strings and board-only patterns:
/// - Full: "lnsgkgsnl/... b P 1" -> mirrors everything
/// - Board-only: "lnsgkgsnl/..." -> mirrors just the board
///
/// # Examples
/// ```
/// use shogi::sfen::mirror_sfen;
///
/// // Board-only pattern
/// let board = "k8/9/9/9/9/9/9/9/9";
/// assert_eq!(mirror_sfen(board), "9/9/9/9/9/9/9/9/8K");
///
/// // Full SFEN with side to move and hands
/// let sfen = "9/9/9/9/9/9/9/9/k8 w P2g 1";
/// assert_eq!(mirror_sfen(sfen), "8K/9/9/9/9/9/9/9/9 b p2G 1");
/// ```
///
/// # Use Cases
/// - Tsumeshogi puzzles where Gote is the attacker (flip so Sente attacks)
/// - Showing positions from the opponent's perspective
/// - Castle study mode for Gote's perspective
pub fn mirror_sfen(sfen: &str) -> String {
    let parts: Vec<&str> = sfen.split_whitespace().collect();

    if parts.is_empty() {
        return sfen.to_string();
    }

    // Mirror the board part
    let mirrored_board = mirror_board(parts[0]);

    // If we only have the board part, return just that
    if parts.len() == 1 {
        return mirrored_board;
    }

    // Flip side-to-move (field 1)
    let flipped_side = if parts[1] == "w" { "b" } else { "w" };

    // Flip hand pieces (field 2) - invert case of all letters
    let flipped_hands = if parts.len() > 2 {
        swap_piece_colors(parts[2])
    } else {
        "-".to_string()
    };

    // Get move number (field 3) or default to 1
    let move_number = if parts.len() > 3 { parts[3] } else { "1" };

    format!(
        "{} {} {} {}",
        mirrored_board, flipped_side, flipped_hands, move_number
    )
}

/// Mirror just the board portion of an SFEN
///
/// Rotates the board 180 degrees and swaps piece colors.
fn mirror_board(board_sfen: &str) -> String {
    let ranks: Vec<&str> = board_sfen.split('/').collect();
    if ranks.len() != 9 {
        // Invalid SFEN board, return as-is
        return board_sfen.to_string();
    }

    // Build a 9x9 grid representation
    let mut grid: Vec<Vec<Option<String>>> = vec![vec![None; 9]; 9];

    // Parse SFEN into grid (handling promoted pieces like +P, +R, etc.)
    for (rank_idx, rank_str) in ranks.iter().enumerate() {
        let mut file_idx = 0;
        let mut chars = rank_str.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch.is_ascii_digit() {
                // Empty squares
                let empty_count = ch.to_digit(10).unwrap() as usize;
                file_idx += empty_count;
            } else if ch == '+' {
                // Promoted piece - combine with next char
                if let Some(piece_char) = chars.next() {
                    grid[rank_idx][file_idx] = Some(format!("+{}", piece_char));
                    file_idx += 1;
                }
            } else {
                // Regular piece
                grid[rank_idx][file_idx] = Some(ch.to_string());
                file_idx += 1;
            }
        }
    }

    // Mirror the grid (flip both horizontally and vertically, swap colors)
    let mut mirrored_grid: Vec<Vec<Option<String>>> = vec![vec![None; 9]; 9];
    for row in 0..9 {
        for col in 0..9 {
            if let Some(piece) = &grid[row][col] {
                let mirrored_row = 8 - row;
                let mirrored_col = 8 - col;

                // Swap piece color
                let mirrored_piece = swap_piece_colors(piece);
                mirrored_grid[mirrored_row][mirrored_col] = Some(mirrored_piece);
            }
        }
    }

    // Convert mirrored grid back to SFEN
    let mut mirrored_sfen = String::new();
    for (rank_idx, rank) in mirrored_grid.iter().enumerate() {
        if rank_idx > 0 {
            mirrored_sfen.push('/');
        }

        let mut empty_count = 0;
        for square in rank {
            if let Some(piece) = square {
                // Flush empty count
                if empty_count > 0 {
                    mirrored_sfen.push_str(&empty_count.to_string());
                    empty_count = 0;
                }
                mirrored_sfen.push_str(piece);
            } else {
                empty_count += 1;
            }
        }
        // Flush remaining empty count
        if empty_count > 0 {
            mirrored_sfen.push_str(&empty_count.to_string());
        }
    }

    mirrored_sfen
}

/// Reset the move number in an SFEN to 1
///
/// Puzzle SFENs often have arbitrary move numbers from the original game.
/// This resets them to 1 for proper ply tracking.
///
/// # Examples
/// ```
/// use shogi::sfen::reset_move_number;
///
/// let sfen = "9/9/9/9/9/9/9/9/k8 b P 71";
/// assert_eq!(reset_move_number(sfen), "9/9/9/9/9/9/9/9/k8 b P 1");
/// ```
pub fn reset_move_number(sfen: &str) -> String {
    let parts: Vec<&str> = sfen.split_whitespace().collect();

    if parts.len() < 4 {
        // Not a full SFEN, return as-is
        return sfen.to_string();
    }

    format!("{} {} {} 1", parts[0], parts[1], parts[2])
}

/// Swap piece colors in a string (uppercase ↔ lowercase)
///
/// Used internally for mirroring operations.
///
/// # Examples
/// ```
/// use shogi::sfen::swap_piece_colors;
///
/// assert_eq!(swap_piece_colors("P2g"), "p2G");
/// assert_eq!(swap_piece_colors("PNBR"), "pnbr");
/// ```
pub fn swap_piece_colors(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_uppercase() {
                c.to_ascii_lowercase()
            } else if c.is_ascii_lowercase() {
                c.to_ascii_uppercase()
            } else {
                c
            }
        })
        .collect()
}

/// Remove a piece from a SFEN board string at the given position
///
/// Returns the modified SFEN with that square empty.
///
/// # Arguments
/// * `sfen_board` - The board portion of a SFEN string (rows separated by '/')
/// * `row` - Row index (0-8, top to bottom)
/// * `col` - Column index (0-8, file 9 to file 1)
///
/// # Examples
/// ```
/// use shogi::sfen::remove_piece_from_sfen;
///
/// let board = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL";
/// let modified = remove_piece_from_sfen(board, 0, 4); // Remove king
/// assert!(modified.starts_with("lnsg1gsnl"));
/// ```
pub fn remove_piece_from_sfen(sfen_board: &str, row: usize, col: usize) -> String {
    let rows: Vec<&str> = sfen_board.split('/').collect();
    let mut result_rows = Vec::new();

    for (row_idx, row_str) in rows.iter().enumerate() {
        if row_idx != row {
            result_rows.push(row_str.to_string());
            continue;
        }

        // Parse this row and remove the piece at col
        let mut current_col = 0;
        let mut new_row = String::new();
        let mut chars = row_str.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch.is_ascii_digit() {
                let empty_count: usize = ch.to_digit(10).unwrap() as usize;
                let end_col = current_col + empty_count;

                if col >= current_col && col < end_col {
                    // The target is within this empty span
                    // Split it: (col - current_col) empty, skip 1, then remaining
                    let before = col - current_col;
                    let after = end_col - col - 1;
                    if before > 0 {
                        new_row.push_str(&before.to_string());
                    }
                    // Add 1 for the "removed" piece (which was already empty, but for consistency)
                    // Actually this case shouldn't happen - we're removing a piece, not an empty square
                    // But handle it gracefully
                    new_row.push('1');
                    if after > 0 {
                        new_row.push_str(&after.to_string());
                    }
                } else {
                    new_row.push(ch);
                }
                current_col = end_col;
            } else if ch == '+' {
                // Promoted piece indicator, combine with next char
                let piece_char = chars.next().unwrap_or('?');
                if current_col == col {
                    // Replace this piece with empty
                    new_row.push('1');
                } else {
                    new_row.push(ch);
                    new_row.push(piece_char);
                }
                current_col += 1;
            } else {
                // Regular piece
                if current_col == col {
                    // Replace this piece with empty
                    new_row.push('1');
                } else {
                    new_row.push(ch);
                }
                current_col += 1;
            }
        }

        // Consolidate consecutive numbers (e.g., "11" -> "2")
        result_rows.push(consolidate_sfen_row(&new_row));
    }

    result_rows.join("/")
}

/// Consolidate consecutive numbers in a SFEN row
///
/// Combines adjacent empty square counts into a single number.
/// For example, "11" becomes "2", "p111P" becomes "p3P".
///
/// # Examples
/// ```
/// use shogi::sfen::consolidate_sfen_row;
///
/// assert_eq!(consolidate_sfen_row("p11P"), "p2P");
/// assert_eq!(consolidate_sfen_row("111"), "3");
/// assert_eq!(consolidate_sfen_row("lnsgkgsnl"), "lnsgkgsnl");
/// ```
pub fn consolidate_sfen_row(row: &str) -> String {
    let mut result = String::new();
    let mut pending_empty = 0;

    for ch in row.chars() {
        if ch.is_ascii_digit() {
            pending_empty += ch.to_digit(10).unwrap() as usize;
        } else {
            if pending_empty > 0 {
                result.push_str(&pending_empty.to_string());
                pending_empty = 0;
            }
            result.push(ch);
        }
    }

    if pending_empty > 0 {
        result.push_str(&pending_empty.to_string());
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sfen_parse_basic() {
        let sfen: Sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
            .parse()
            .unwrap();

        assert_eq!(
            sfen.board(),
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL"
        );
        assert_eq!(sfen.side_to_move(), Color::Black);
        assert_eq!(sfen.hand(), "-");
        assert_eq!(sfen.ply(), 1);
        assert!(!sfen.has_moves());
    }

    #[test]
    fn test_sfen_parse_with_hand() {
        let sfen: Sfen = "9/9/9/9/4k4/9/9/9/4K4 w P2g 10".parse().unwrap();

        assert_eq!(sfen.side_to_move(), Color::White);
        assert_eq!(sfen.hand(), "P2g");
        assert_eq!(sfen.ply(), 10);
    }

    #[test]
    fn test_sfen_parse_with_moves() {
        let sfen: Sfen =
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f 3c3d"
                .parse()
                .unwrap();

        assert!(sfen.has_moves());
        assert_eq!(sfen.moves().len(), 2);
        assert_eq!(sfen.moves()[0], "7g7f");
        assert_eq!(sfen.moves()[1], "3c3d");
    }

    #[test]
    fn test_sfen_display_basic() {
        let sfen = Sfen::new(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL".to_string(),
            Color::Black,
            "-".to_string(),
            1,
        );

        assert_eq!(
            sfen.to_string(),
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
        );
    }

    #[test]
    fn test_sfen_display_with_moves() {
        let sfen = Sfen::with_moves(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL".to_string(),
            Color::Black,
            "-".to_string(),
            1,
            vec!["7g7f".to_string(), "3c3d".to_string()],
        );

        assert_eq!(
            sfen.to_string(),
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f 3c3d"
        );
    }

    #[test]
    fn test_sfen_roundtrip() {
        let original = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
        let sfen: Sfen = original.parse().unwrap();
        assert_eq!(sfen.to_string(), original);
    }

    #[test]
    fn test_sfen_roundtrip_with_moves() {
        let original =
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f 3c3d";
        let sfen: Sfen = original.parse().unwrap();
        assert_eq!(sfen.to_string(), original);
    }

    #[test]
    fn test_sfen_startpos() {
        let sfen = Sfen::startpos();
        assert_eq!(
            sfen.to_string(),
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
        );
    }

    #[test]
    fn test_sfen_parse_error_missing_fields() {
        assert!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL"
            .parse::<Sfen>()
            .is_err());
        assert!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b"
            .parse::<Sfen>()
            .is_err());
        assert!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b -"
            .parse::<Sfen>()
            .is_err());
    }

    #[test]
    fn test_sfen_parse_error_invalid_stm() {
        assert!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL x - 1"
            .parse::<Sfen>()
            .is_err());
    }

    #[test]
    fn test_mirror_sfen_board_only() {
        // Board-only pattern: king in top-left ends up in bottom-right with swapped color
        let board = "k8/9/9/9/9/9/9/9/9";
        let mirrored = mirror_sfen(board);
        assert_eq!(mirrored, "9/9/9/9/9/9/9/9/8K");
    }

    #[test]
    fn test_mirror_sfen_full() {
        // Full SFEN: board + side + hands + move number
        let sfen = "9/9/9/9/9/9/9/9/k8 w - 1";
        let mirrored = mirror_sfen(sfen);
        assert_eq!(mirrored, "8K/9/9/9/9/9/9/9/9 b - 1");
    }

    #[test]
    fn test_mirror_sfen_with_hands() {
        // SFEN with pieces in hand
        let sfen = "9/9/9/9/9/9/9/9/k8 w P2g 1";
        let mirrored = mirror_sfen(sfen);
        assert_eq!(mirrored, "8K/9/9/9/9/9/9/9/9 b p2G 1");
    }

    #[test]
    fn test_reset_move_number() {
        // Puzzle SFENs have arbitrary move numbers
        let sfen = "9/9/9/9/9/9/9/9/k8 b P 71";
        let reset = reset_move_number(sfen);
        assert_eq!(reset, "9/9/9/9/9/9/9/9/k8 b P 1");
    }

    #[test]
    fn test_reset_move_number_already_one() {
        // Should work even if already 1
        let sfen = "9/9/9/9/9/9/9/9/k8 b - 1";
        let reset = reset_move_number(sfen);
        assert_eq!(reset, "9/9/9/9/9/9/9/9/k8 b - 1");
    }

    #[test]
    fn test_consolidate_sfen_row() {
        assert_eq!(consolidate_sfen_row("p11P"), "p2P");
        assert_eq!(consolidate_sfen_row("111"), "3");
        assert_eq!(consolidate_sfen_row("lnsgkgsnl"), "lnsgkgsnl");
        assert_eq!(consolidate_sfen_row("1r5b1"), "1r5b1");
        assert_eq!(consolidate_sfen_row("9"), "9");
    }

    #[test]
    fn test_remove_piece_from_sfen() {
        // Remove king from starting position
        let board = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL";
        let modified = remove_piece_from_sfen(board, 0, 4);
        assert!(modified.starts_with("lnsg1gsnl"));

        // Remove piece from middle of row
        let modified = remove_piece_from_sfen(board, 0, 0);
        assert!(modified.starts_with("1nsgkgsnl"));

        // Remove piece from end of row
        let modified = remove_piece_from_sfen(board, 0, 8);
        assert!(modified.starts_with("lnsgkgsn1"));
    }

    #[test]
    fn test_remove_piece_consolidates() {
        // Removing adjacent to empty squares should consolidate
        let board = "l1sgkgsnl/9/9/9/9/9/9/9/9";
        let modified = remove_piece_from_sfen(board, 0, 0);
        // "l" removed, should become "2sgkgsnl" (1+1=2)
        assert!(modified.starts_with("2sgkgsnl"));
    }

    #[test]
    fn test_swap_piece_colors() {
        assert_eq!(swap_piece_colors("P"), "p");
        assert_eq!(swap_piece_colors("p"), "P");
        assert_eq!(swap_piece_colors("P2g"), "p2G");
        assert_eq!(swap_piece_colors("-"), "-");
        assert_eq!(swap_piece_colors("+P"), "+p");
    }
}