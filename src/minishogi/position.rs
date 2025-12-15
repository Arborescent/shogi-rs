//! Mini Shogi position (game state).

use crate::Color;
use crate::error::{MoveError, SfenError};

use super::moves::{Move, MoveRecord};
use super::{is_valid_piece_type, is_valid_hand_piece, Hand, Piece, PieceType, Square};
use super::{BOARD_HEIGHT, BOARD_WIDTH, NUM_SQUARES, PROMOTION_ZONE_DEPTH};

/// Piece grid for the 5×5 board.
#[derive(Clone, Debug)]
struct PieceGrid([Option<Piece>; NUM_SQUARES]);

impl PieceGrid {
    fn empty() -> Self {
        PieceGrid([None; NUM_SQUARES])
    }

    fn get(&self, sq: Square) -> Option<Piece> {
        self.0[sq.index()]
    }

    fn set(&mut self, sq: Square, pc: Option<Piece>) {
        self.0[sq.index()] = pc;
    }
}

/// Result of checking game status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameResult {
    /// Game is still in progress.
    InProgress,
    /// The specified color wins.
    Win(Color),
    /// Draw (stalemate).
    Draw,
}

/// Represents the game state for Mini Shogi.
#[derive(Debug, Clone)]
pub struct Position {
    board: PieceGrid,
    hand: Hand,
    side_to_move: Color,
    ply: u16,
    move_history: Vec<MoveRecord>,
}

impl Default for Position {
    fn default() -> Self {
        Position::new()
    }
}

impl Position {
    /// Creates a new position with an empty board.
    pub fn new() -> Self {
        Position {
            board: PieceGrid::empty(),
            hand: Hand::default(),
            side_to_move: Color::Black,
            ply: 1,
            move_history: Vec::new(),
        }
    }

    /// Creates a position with the standard starting setup.
    pub fn startpos() -> Self {
        let mut pos = Position::new();
        pos.set_sfen(super::STARTING_SFEN).expect("Invalid starting SFEN");
        pos
    }

    // =========================================================================
    // Accessors
    // =========================================================================

    /// Returns the piece at the given square.
    pub fn piece_at(&self, sq: Square) -> Option<Piece> {
        self.board.get(sq)
    }

    /// Returns the current side to move.
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Returns the current ply count.
    pub fn ply(&self) -> u16 {
        self.ply
    }

    /// Returns the number of pieces in hand.
    pub fn hand(&self, p: Piece) -> u8 {
        self.hand.get(p)
    }

    /// Returns the move history.
    pub fn move_history(&self) -> &[MoveRecord] {
        &self.move_history
    }

    /// Finds the King's square for the given color.
    pub fn find_king(&self, color: Color) -> Option<Square> {
        for sq in Square::iter() {
            if let Some(p) = self.board.get(sq) {
                if p.piece_type == PieceType::King && p.color == color {
                    return Some(sq);
                }
            }
        }
        None
    }

    // =========================================================================
    // SFEN
    // =========================================================================

    /// Sets the position from a SFEN string.
    ///
    /// Format: `rbsgk/4p/5/P4/KGSBR b - 1`
    pub fn set_sfen(&mut self, sfen: &str) -> Result<(), SfenError> {
        let parts: Vec<&str> = sfen.split_whitespace().collect();
        if parts.len() < 4 {
            return Err(SfenError::MissingDataFields);
        }

        // Clear board
        self.board = PieceGrid::empty();
        self.hand.clear();
        self.move_history.clear();

        // Parse board
        let ranks: Vec<&str> = parts[0].split('/').collect();
        if ranks.len() != BOARD_HEIGHT as usize {
            return Err(SfenError::IllegalBoardState);
        }

        for (rank_idx, rank_str) in ranks.iter().enumerate() {
            let mut file = 0u8;
            let mut chars = rank_str.chars().peekable();

            while let Some(c) = chars.next() {
                if file > BOARD_WIDTH {
                    return Err(SfenError::IllegalBoardState);
                }

                if c.is_ascii_digit() {
                    file += c.to_digit(10).unwrap() as u8;
                } else if c == '+' {
                    // Promoted piece
                    if let Some(next_c) = chars.next() {
                        let base_piece = Piece::from_sfen(next_c)
                            .ok_or(SfenError::IllegalPieceType)?;
                        if !is_valid_piece_type(base_piece.piece_type) {
                            return Err(SfenError::IllegalPieceType);
                        }
                        let promoted = base_piece.promote()
                            .ok_or(SfenError::IllegalPieceType)?;
                        let sq = Square::new(file, rank_idx as u8)
                            .ok_or(SfenError::IllegalBoardState)?;
                        self.board.set(sq, Some(promoted));
                        file += 1;
                    }
                } else {
                    let piece = Piece::from_sfen(c)
                        .ok_or(SfenError::IllegalPieceType)?;
                    if !is_valid_piece_type(piece.piece_type) {
                        return Err(SfenError::IllegalPieceType);
                    }
                    let sq = Square::new(file, rank_idx as u8)
                        .ok_or(SfenError::IllegalBoardState)?;
                    self.board.set(sq, Some(piece));
                    file += 1;
                }
            }
        }

        // Parse side to move
        self.side_to_move = match parts[1] {
            "b" => Color::Black,
            "w" => Color::White,
            _ => return Err(SfenError::IllegalSideToMove),
        };

        // Parse hand
        if parts[2] != "-" {
            let mut chars = parts[2].chars().peekable();
            while let Some(c) = chars.next() {
                if c.is_ascii_digit() {
                    let count = c.to_digit(10).unwrap() as u8;
                    if let Some(piece_char) = chars.next() {
                        let piece = Piece::from_sfen(piece_char)
                            .ok_or(SfenError::IllegalPieceType)?;
                        if !is_valid_hand_piece(piece.piece_type) {
                            return Err(SfenError::IllegalPieceType);
                        }
                        self.hand.set(piece, count);
                    }
                } else {
                    let piece = Piece::from_sfen(c)
                        .ok_or(SfenError::IllegalPieceType)?;
                    if !is_valid_hand_piece(piece.piece_type) {
                        return Err(SfenError::IllegalPieceType);
                    }
                    let current = self.hand.get(piece);
                    self.hand.set(piece, current + 1);
                }
            }
        }

        // Parse ply
        self.ply = parts[3].parse()?;

        Ok(())
    }

    /// Returns the SFEN representation of this position.
    pub fn to_sfen(&self) -> String {
        let mut sfen = String::new();

        // Board
        for rank in 0..BOARD_HEIGHT {
            let mut empty_count = 0;
            for file in 0..BOARD_WIDTH {
                let sq = Square::new(file, rank).unwrap();
                if let Some(piece) = self.board.get(sq) {
                    if empty_count > 0 {
                        sfen.push_str(&empty_count.to_string());
                        empty_count = 0;
                    }
                    sfen.push_str(&piece.to_string());
                } else {
                    empty_count += 1;
                }
            }
            if empty_count > 0 {
                sfen.push_str(&empty_count.to_string());
            }
            if rank < BOARD_HEIGHT - 1 {
                sfen.push('/');
            }
        }

        // Side to move
        sfen.push(' ');
        sfen.push(if self.side_to_move == Color::Black { 'b' } else { 'w' });

        // Hand
        sfen.push(' ');
        let mut hand_str = String::new();
        for color in [Color::Black, Color::White] {
            for pt in [PieceType::Rook, PieceType::Bishop, PieceType::Gold, PieceType::Silver, PieceType::Pawn] {
                let piece = Piece { piece_type: pt, color };
                let count = self.hand.get(piece);
                if count > 0 {
                    if count > 1 {
                        hand_str.push_str(&count.to_string());
                    }
                    hand_str.push_str(&piece.to_string());
                }
            }
        }
        if hand_str.is_empty() {
            sfen.push('-');
        } else {
            sfen.push_str(&hand_str);
        }

        // Ply
        sfen.push(' ');
        sfen.push_str(&self.ply.to_string());

        // Move history
        if !self.move_history.is_empty() {
            sfen.push_str(" moves");
            for mv in &self.move_history {
                sfen.push(' ');
                sfen.push_str(&mv.to_sfen());
            }
        }

        sfen
    }

    // =========================================================================
    // Move execution
    // =========================================================================

    /// Makes a move on the position.
    pub fn make_move(&mut self, m: Move) -> Result<(), MoveError> {
        match m {
            Move::Normal { from, to, promote } => {
                self.make_normal_move(from, to, promote)?;
            }
            Move::Drop { to, piece_type } => {
                self.make_drop_move(to, piece_type)?;
            }
        }

        self.side_to_move = self.side_to_move.flip();
        self.ply += 1;

        Ok(())
    }

    fn make_normal_move(&mut self, from: Square, to: Square, promote: bool) -> Result<(), MoveError> {
        let piece = self.board.get(from).ok_or(MoveError::Inconsistent("no piece at from square"))?;

        if piece.color != self.side_to_move {
            return Err(MoveError::EnemysTurn);
        }

        let captured = self.board.get(to);

        // Handle capture
        if let Some(cap) = captured {
            if cap.color == self.side_to_move {
                return Err(MoveError::Inconsistent("cannot capture own piece"));
            }
            // Add to hand (demotes promoted pieces)
            let hand_piece = cap.unpromote().unwrap_or(cap).flip();
            if is_valid_hand_piece(hand_piece.piece_type) {
                self.hand.increment(hand_piece);
            }
        }

        // Handle promotion
        let in_promo_zone_from = from.relative_rank(piece.color) < PROMOTION_ZONE_DEPTH;
        let in_promo_zone_to = to.relative_rank(piece.color) < PROMOTION_ZONE_DEPTH;
        let can_promote = piece.piece_type.promote().is_some() && (in_promo_zone_from || in_promo_zone_to);

        if promote && !can_promote {
            return Err(MoveError::Inconsistent("piece cannot promote"));
        }

        // Check if must promote (pawn on last rank)
        let must_promote = piece.piece_type == PieceType::Pawn && to.relative_rank(piece.color) == 0;

        let placed = if promote || must_promote {
            piece.promote().ok_or(MoveError::Inconsistent("piece cannot promote"))?
        } else {
            piece
        };

        self.board.set(from, None);
        self.board.set(to, Some(placed));

        // Check if move leaves own king in check
        if self.is_in_check(self.side_to_move) {
            // Undo the move
            self.board.set(from, Some(piece));
            self.board.set(to, captured);
            if let Some(cap) = captured {
                let hand_piece = cap.unpromote().unwrap_or(cap).flip();
                if is_valid_hand_piece(hand_piece.piece_type) {
                    self.hand.decrement(hand_piece);
                }
            }
            return Err(MoveError::InCheck);
        }

        self.move_history.push(MoveRecord::Normal {
            from,
            to,
            placed,
            captured,
            promoted: promote || must_promote,
        });

        Ok(())
    }

    fn make_drop_move(&mut self, to: Square, piece_type: PieceType) -> Result<(), MoveError> {
        if self.board.get(to).is_some() {
            return Err(MoveError::Inconsistent("square is occupied"));
        }

        if !is_valid_hand_piece(piece_type) {
            return Err(MoveError::Inconsistent("invalid hand piece type"));
        }

        let piece = Piece { piece_type, color: self.side_to_move };

        if self.hand.get(piece) == 0 {
            return Err(MoveError::Inconsistent("piece not in hand"));
        }

        // Check placement restrictions
        // Pawn cannot be dropped on last rank
        if piece_type == PieceType::Pawn && to.relative_rank(self.side_to_move) == 0 {
            return Err(MoveError::NonMovablePiece);
        }

        // Nifu: cannot drop pawn on a file that already has an unpromoted pawn
        if piece_type == PieceType::Pawn {
            for rank in 0..BOARD_HEIGHT {
                if let Some(sq) = Square::new(to.file(), rank) {
                    if let Some(p) = self.board.get(sq) {
                        if p.piece_type == PieceType::Pawn && p.color == self.side_to_move {
                            return Err(MoveError::Nifu);
                        }
                    }
                }
            }
        }

        // Uchifuzume: cannot drop pawn to give immediate checkmate
        if piece_type == PieceType::Pawn {
            // Temporarily place the pawn
            self.board.set(to, Some(piece));
            self.hand.decrement(piece);

            let opponent = self.side_to_move.flip();
            let gives_check = self.is_in_check(opponent);

            if gives_check {
                // Check if it's checkmate
                let is_checkmate = self.is_checkmate_for(opponent);

                if is_checkmate {
                    // Undo and return error
                    self.board.set(to, None);
                    self.hand.increment(piece);
                    return Err(MoveError::Uchifuzume);
                }
            }

            // Undo temporary placement for now
            self.board.set(to, None);
            self.hand.increment(piece);
        }

        // Actually make the drop
        self.hand.decrement(piece);
        self.board.set(to, Some(piece));

        // Check if drop leaves own king in check (shouldn't happen but check anyway)
        if self.is_in_check(self.side_to_move) {
            self.board.set(to, None);
            self.hand.increment(piece);
            return Err(MoveError::InCheck);
        }

        self.move_history.push(MoveRecord::Drop { to, piece });

        Ok(())
    }

    /// Helper to check if opponent is in checkmate (for uchifuzume check).
    fn is_checkmate_for(&self, color: Color) -> bool {
        // Clone and set side to move to the color we're checking
        let mut test_pos = self.clone();
        test_pos.side_to_move = color;
        test_pos.legal_moves().is_empty() && test_pos.is_in_check(color)
    }

    /// Undoes the last move.
    pub fn unmake_move(&mut self) -> Result<(), MoveError> {
        let record = self.move_history.pop().ok_or(MoveError::Inconsistent("no move to undo"))?;

        match record {
            MoveRecord::Normal {
                from,
                to,
                placed,
                captured,
                promoted,
            } => {
                // Restore piece to original position
                let original_piece = if promoted {
                    placed.unpromote().unwrap_or(placed)
                } else {
                    placed
                };
                self.board.set(from, Some(original_piece));
                self.board.set(to, captured);

                // Remove from hand if piece was captured
                if let Some(cap) = captured {
                    let hand_piece = cap.unpromote().unwrap_or(cap).flip();
                    if is_valid_hand_piece(hand_piece.piece_type) {
                        self.hand.decrement(hand_piece);
                    }
                }
            }
            MoveRecord::Drop { to, piece } => {
                self.board.set(to, None);
                self.hand.increment(piece);
            }
        }

        self.side_to_move = self.side_to_move.flip();
        self.ply -= 1;

        Ok(())
    }

    // =========================================================================
    // Move generation
    // =========================================================================

    /// Returns all legal moves for the current side.
    pub fn legal_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

        // Generate piece moves
        for sq in Square::iter() {
            if let Some(piece) = self.board.get(sq) {
                if piece.color == self.side_to_move {
                    self.generate_piece_moves(sq, piece, &mut moves);
                }
            }
        }

        // Generate drop moves
        for pt in [PieceType::Rook, PieceType::Bishop, PieceType::Gold, PieceType::Silver, PieceType::Pawn] {
            let piece = Piece { piece_type: pt, color: self.side_to_move };
            if self.hand.get(piece) > 0 {
                for sq in Square::iter() {
                    if self.board.get(sq).is_none() {
                        // Check placement restrictions
                        if pt == PieceType::Pawn {
                            // Last rank restriction
                            if sq.relative_rank(self.side_to_move) == 0 {
                                continue;
                            }
                            // Nifu check
                            let mut has_pawn = false;
                            for rank in 0..BOARD_HEIGHT {
                                if let Some(check_sq) = Square::new(sq.file(), rank) {
                                    if let Some(p) = self.board.get(check_sq) {
                                        if p.piece_type == PieceType::Pawn && p.color == self.side_to_move {
                                            has_pawn = true;
                                            break;
                                        }
                                    }
                                }
                            }
                            if has_pawn {
                                continue;
                            }
                        }
                        moves.push(Move::Drop { to: sq, piece_type: pt });
                    }
                }
            }
        }

        // Filter out moves that leave our King in check or are uchifuzume
        moves.retain(|&m| {
            let mut test_pos = self.clone();
            test_pos.make_move(m).is_ok()
        });

        moves
    }

    fn generate_piece_moves(&self, from: Square, piece: Piece, moves: &mut Vec<Move>) {
        let pt = piece.piece_type;

        // Sliding pieces: Rook, Bishop, Dragon (ProRook), Horse (ProBishop)
        if matches!(pt, PieceType::Rook | PieceType::ProRook | PieceType::Bishop | PieceType::ProBishop) {
            self.generate_sliding_moves(from, piece, moves);
        } else {
            // Stepping pieces
            self.generate_stepping_moves(from, piece, moves);
        }
    }

    fn generate_sliding_moves(&self, from: Square, piece: Piece, moves: &mut Vec<Move>) {
        let pt = piece.piece_type;

        // Get sliding directions
        let slide_dirs: &[(i8, i8)] = match pt {
            PieceType::Rook => &[(0, -1), (0, 1), (-1, 0), (1, 0)],
            PieceType::Bishop => &[(-1, -1), (1, -1), (-1, 1), (1, 1)],
            PieceType::ProRook => &[(0, -1), (0, 1), (-1, 0), (1, 0)], // Plus King moves
            PieceType::ProBishop => &[(-1, -1), (1, -1), (-1, 1), (1, 1)], // Plus King moves
            _ => &[],
        };

        // Slide in each direction
        for &(df, dr) in slide_dirs {
            let mut current = from;
            loop {
                match current.shift(df, dr) {
                    Some(to) => {
                        if let Some(target) = self.board.get(to) {
                            if target.color != piece.color {
                                // Can capture
                                self.add_move_with_promotion(from, to, piece, moves);
                            }
                            break; // Blocked
                        } else {
                            // Empty square
                            self.add_move_with_promotion(from, to, piece, moves);
                            current = to;
                        }
                    }
                    None => break, // Off board
                }
            }
        }

        // Add step moves for promoted pieces (Dragon gets King moves, Horse gets King moves)
        let step_dirs: &[(i8, i8)] = match pt {
            PieceType::ProRook => &[(-1, -1), (1, -1), (-1, 1), (1, 1)], // Diagonal steps
            PieceType::ProBishop => &[(0, -1), (0, 1), (-1, 0), (1, 0)], // Orthogonal steps
            _ => &[],
        };

        for &(df, dr) in step_dirs {
            if let Some(to) = from.shift(df, dr) {
                if let Some(target) = self.board.get(to) {
                    if target.color == piece.color {
                        continue; // Can't capture own piece
                    }
                }
                self.add_move_with_promotion(from, to, piece, moves);
            }
        }
    }

    fn generate_stepping_moves(&self, from: Square, piece: Piece, moves: &mut Vec<Move>) {
        let directions = self.get_step_directions(piece.piece_type, piece.color);

        for (df, dr) in directions {
            if let Some(to) = from.shift(df, dr) {
                if let Some(target) = self.board.get(to) {
                    if target.color == piece.color {
                        continue; // Can't capture own piece
                    }
                }
                self.add_move_with_promotion(from, to, piece, moves);
            }
        }
    }

    fn add_move_with_promotion(&self, from: Square, to: Square, piece: Piece, moves: &mut Vec<Move>) {
        let in_promo_zone_from = from.relative_rank(piece.color) < PROMOTION_ZONE_DEPTH;
        let in_promo_zone_to = to.relative_rank(piece.color) < PROMOTION_ZONE_DEPTH;
        let can_promote = piece.piece_type.promote().is_some() && (in_promo_zone_from || in_promo_zone_to);
        let must_promote = piece.piece_type == PieceType::Pawn && to.relative_rank(piece.color) == 0;

        if must_promote {
            moves.push(Move::Normal { from, to, promote: true });
        } else if can_promote {
            // Add both promotion and non-promotion moves
            moves.push(Move::Normal { from, to, promote: true });
            moves.push(Move::Normal { from, to, promote: false });
        } else {
            moves.push(Move::Normal { from, to, promote: false });
        }
    }

    fn get_step_directions(&self, pt: PieceType, color: Color) -> Vec<(i8, i8)> {
        let forward = if color == Color::Black { -1 } else { 1 };

        match pt {
            PieceType::King => vec![
                (-1, -1), (0, -1), (1, -1),
                (-1, 0),          (1, 0),
                (-1, 1),  (0, 1),  (1, 1),
            ],
            PieceType::Gold | PieceType::ProSilver | PieceType::ProPawn => vec![
                (-1, forward), (0, forward), (1, forward),
                (-1, 0),                      (1, 0),
                (0, -forward),
            ],
            PieceType::Silver => vec![
                (-1, forward), (0, forward), (1, forward),
                (-1, -forward),              (1, -forward),
            ],
            PieceType::Pawn => vec![(0, forward)],
            // Rook, Bishop handled by sliding
            _ => vec![],
        }
    }

    // =========================================================================
    // Check detection
    // =========================================================================

    /// Returns true if the specified color's King is in check.
    pub fn is_in_check(&self, color: Color) -> bool {
        let king_sq = match self.find_king(color) {
            Some(sq) => sq,
            None => return false, // No King = already captured
        };

        self.is_attacked_by(king_sq, color.flip())
    }

    /// Returns true if the square is attacked by the given color.
    pub fn is_attacked_by(&self, sq: Square, attacker: Color) -> bool {
        for from_sq in Square::iter() {
            if let Some(piece) = self.board.get(from_sq) {
                if piece.color == attacker {
                    if self.can_attack(from_sq, sq, piece) {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn can_attack(&self, from: Square, to: Square, piece: Piece) -> bool {
        let pt = piece.piece_type;

        // Check sliding pieces
        if matches!(pt, PieceType::Rook | PieceType::ProRook) {
            if self.can_slide_attack(from, to, &[(0, -1), (0, 1), (-1, 0), (1, 0)]) {
                return true;
            }
        }
        if matches!(pt, PieceType::Bishop | PieceType::ProBishop) {
            if self.can_slide_attack(from, to, &[(-1, -1), (1, -1), (-1, 1), (1, 1)]) {
                return true;
            }
        }

        // Check step attacks for promoted pieces
        if pt == PieceType::ProRook {
            // Dragon can also step diagonally
            let df = to.file() as i8 - from.file() as i8;
            let dr = to.rank() as i8 - from.rank() as i8;
            if df.abs() == 1 && dr.abs() == 1 {
                return true;
            }
        }
        if pt == PieceType::ProBishop {
            // Horse can also step orthogonally
            let df = to.file() as i8 - from.file() as i8;
            let dr = to.rank() as i8 - from.rank() as i8;
            if (df == 0 && dr.abs() == 1) || (df.abs() == 1 && dr == 0) {
                return true;
            }
        }

        // Check stepping pieces
        let directions = self.get_step_directions(pt, piece.color);
        for (df, dr) in directions {
            if let Some(target) = from.shift(df, dr) {
                if target == to {
                    return true;
                }
            }
        }

        false
    }

    fn can_slide_attack(&self, from: Square, to: Square, dirs: &[(i8, i8)]) -> bool {
        let df = (to.file() as i8 - from.file() as i8).signum();
        let dr = (to.rank() as i8 - from.rank() as i8).signum();

        // Check if direction is valid for this sliding move
        if !dirs.contains(&(df, dr)) {
            return false;
        }

        // Check if on same line
        let file_diff = (to.file() as i8 - from.file() as i8).abs();
        let rank_diff = (to.rank() as i8 - from.rank() as i8).abs();

        let on_line = (df == 0 && rank_diff > 0) || (dr == 0 && file_diff > 0) ||
                      (file_diff == rank_diff && file_diff > 0);

        if !on_line {
            return false;
        }

        // Check for obstacles
        let mut current = from;
        loop {
            match current.shift(df, dr) {
                Some(next) => {
                    if next == to {
                        return true;
                    }
                    if self.board.get(next).is_some() {
                        return false; // Blocked
                    }
                    current = next;
                }
                None => return false,
            }
        }
    }

    // =========================================================================
    // Win condition checking
    // =========================================================================

    /// Checks the game result.
    ///
    /// Win conditions:
    /// 1. Checkmate: No legal moves and King is in check
    /// 2. Stalemate: No legal moves (draw in Mini Shogi)
    pub fn game_result(&self) -> GameResult {
        // Check if a King is missing (was captured)
        if self.find_king(Color::Black).is_none() {
            return GameResult::Win(Color::White);
        }
        if self.find_king(Color::White).is_none() {
            return GameResult::Win(Color::Black);
        }

        // Check for checkmate/stalemate
        if self.legal_moves().is_empty() {
            if self.is_in_check(self.side_to_move) {
                // Checkmate
                return GameResult::Win(self.side_to_move.flip());
            } else {
                // Stalemate - draw in Mini Shogi
                return GameResult::Draw;
            }
        }

        GameResult::InProgress
    }

    /// Returns true if the game is over.
    pub fn is_game_over(&self) -> bool {
        !matches!(self.game_result(), GameResult::InProgress)
    }

    /// Returns the winner if the game is over.
    pub fn winner(&self) -> Option<Color> {
        match self.game_result() {
            GameResult::Win(c) => Some(c),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_position() {
        let pos = Position::new();
        assert_eq!(Color::Black, pos.side_to_move());
        assert_eq!(1, pos.ply());
    }

    #[test]
    fn startpos() {
        let pos = Position::startpos();
        assert_eq!(Color::Black, pos.side_to_move());

        // Check pieces are in correct positions
        // Top row (White): Rook-Bishop-Silver-Gold-King
        let sq_1a = Square::new(0, 0).unwrap();
        let sq_2a = Square::new(1, 0).unwrap();
        let sq_3a = Square::new(2, 0).unwrap();
        let sq_4a = Square::new(3, 0).unwrap();
        let sq_5a = Square::new(4, 0).unwrap();

        assert_eq!(Some(Piece { piece_type: PieceType::Rook, color: Color::White }), pos.piece_at(sq_1a));
        assert_eq!(Some(Piece { piece_type: PieceType::Bishop, color: Color::White }), pos.piece_at(sq_2a));
        assert_eq!(Some(Piece { piece_type: PieceType::Silver, color: Color::White }), pos.piece_at(sq_3a));
        assert_eq!(Some(Piece { piece_type: PieceType::Gold, color: Color::White }), pos.piece_at(sq_4a));
        assert_eq!(Some(Piece { piece_type: PieceType::King, color: Color::White }), pos.piece_at(sq_5a));

        // Second row: White Pawn at 5b
        let sq_5b = Square::new(4, 1).unwrap();
        assert_eq!(Some(Piece { piece_type: PieceType::Pawn, color: Color::White }), pos.piece_at(sq_5b));

        // Fourth row: Black Pawn at 1d
        let sq_1d = Square::new(0, 3).unwrap();
        assert_eq!(Some(Piece { piece_type: PieceType::Pawn, color: Color::Black }), pos.piece_at(sq_1d));

        // Bottom row (Black): King-Gold-Silver-Bishop-Rook
        let sq_1e = Square::new(0, 4).unwrap();
        let sq_2e = Square::new(1, 4).unwrap();
        let sq_3e = Square::new(2, 4).unwrap();
        let sq_4e = Square::new(3, 4).unwrap();
        let sq_5e = Square::new(4, 4).unwrap();

        assert_eq!(Some(Piece { piece_type: PieceType::King, color: Color::Black }), pos.piece_at(sq_1e));
        assert_eq!(Some(Piece { piece_type: PieceType::Gold, color: Color::Black }), pos.piece_at(sq_2e));
        assert_eq!(Some(Piece { piece_type: PieceType::Silver, color: Color::Black }), pos.piece_at(sq_3e));
        assert_eq!(Some(Piece { piece_type: PieceType::Bishop, color: Color::Black }), pos.piece_at(sq_4e));
        assert_eq!(Some(Piece { piece_type: PieceType::Rook, color: Color::Black }), pos.piece_at(sq_5e));
    }

    #[test]
    fn sfen_roundtrip() {
        let pos = Position::startpos();
        let sfen = pos.to_sfen();

        let mut pos2 = Position::new();
        pos2.set_sfen(&sfen).unwrap();

        assert_eq!(pos.side_to_move(), pos2.side_to_move());
        assert_eq!(pos.ply(), pos2.ply());

        for sq in Square::iter() {
            assert_eq!(pos.piece_at(sq), pos2.piece_at(sq));
        }
    }

    #[test]
    fn make_normal_move() {
        let mut pos = Position::startpos();

        // Move Black's Pawn forward
        let from = Square::new(0, 3).unwrap();
        let to = Square::new(0, 2).unwrap();
        let mv = Move::Normal { from, to, promote: false };

        pos.make_move(mv).unwrap();

        assert_eq!(None, pos.piece_at(from));
        assert_eq!(Some(Piece { piece_type: PieceType::Pawn, color: Color::Black }), pos.piece_at(to));
        assert_eq!(Color::White, pos.side_to_move());
    }

    #[test]
    fn make_drop_move() {
        let mut pos = Position::new();
        pos.set_sfen("rbsgk/4p/5/P4/KGSBR b P 1").unwrap();

        let to = Square::new(2, 2).unwrap();
        let mv = Move::Drop { to, piece_type: PieceType::Pawn };

        pos.make_move(mv).unwrap();

        assert_eq!(Some(Piece { piece_type: PieceType::Pawn, color: Color::Black }), pos.piece_at(to));
        assert_eq!(0, pos.hand(Piece { piece_type: PieceType::Pawn, color: Color::Black }));
    }

    #[test]
    fn unmake_move() {
        let mut pos = Position::startpos();
        let original_sfen = pos.to_sfen();

        let from = Square::new(0, 3).unwrap();
        let to = Square::new(0, 2).unwrap();
        let mv = Move::Normal { from, to, promote: false };

        pos.make_move(mv).unwrap();
        pos.unmake_move().unwrap();

        // Position should be restored (excluding move history in SFEN)
        let restored_sfen = pos.to_sfen();
        let restored_parts: Vec<&str> = restored_sfen.split(" moves").next().unwrap().split_whitespace().collect();
        let original_parts: Vec<&str> = original_sfen.split_whitespace().collect();

        assert_eq!(original_parts[0], restored_parts[0]); // Board
        assert_eq!(original_parts[1], restored_parts[1]); // Side to move
        assert_eq!(original_parts[2], restored_parts[2]); // Hand
    }

    #[test]
    fn legal_moves_starting_position() {
        let pos = Position::startpos();
        let moves = pos.legal_moves();

        // Starting position should have legal moves
        assert!(!moves.is_empty());

        // Should include Pawn moving forward
        let pawn_move = Move::Normal {
            from: Square::new(0, 3).unwrap(),
            to: Square::new(0, 2).unwrap(),
            promote: false,
        };
        assert!(moves.contains(&pawn_move));
    }

    #[test]
    fn check_detection() {
        let mut pos = Position::new();
        // Position where White King is in check by Black Rook (same file)
        pos.set_sfen("4k/5/5/5/K3R b - 1").unwrap();

        assert!(pos.is_in_check(Color::White));
        assert!(!pos.is_in_check(Color::Black));
    }

    #[test]
    fn game_not_over_at_start() {
        let pos = Position::startpos();
        assert_eq!(GameResult::InProgress, pos.game_result());
        assert!(!pos.is_game_over());
        assert_eq!(None, pos.winner());
    }

    #[test]
    fn nifu_prevented() {
        let mut pos = Position::new();
        // Black has a Pawn on file 1, and a Pawn in hand
        pos.set_sfen("4k/5/P4/5/K4 b P 1").unwrap();

        // Try to drop pawn on file 1 (should be rejected)
        let mv = Move::Drop {
            to: Square::new(0, 3).unwrap(),
            piece_type: PieceType::Pawn,
        };

        assert!(pos.make_move(mv).is_err());
    }

    #[test]
    fn pawn_last_rank_drop_prevented() {
        let mut pos = Position::new();
        pos.set_sfen("4k/5/5/5/K4 b P 1").unwrap();

        // Try to drop pawn on last rank (rank 0 for Black)
        let mv = Move::Drop {
            to: Square::new(1, 0).unwrap(),
            piece_type: PieceType::Pawn,
        };

        assert!(pos.make_move(mv).is_err());
    }

    #[test]
    fn sliding_piece_moves() {
        let mut pos = Position::new();
        pos.set_sfen("4k/5/5/5/R3K b - 1").unwrap();

        let moves = pos.legal_moves();

        // Rook should be able to move along file 1 and rank 5
        let rook_move = Move::Normal {
            from: Square::new(0, 4).unwrap(),
            to: Square::new(0, 0).unwrap(), // All the way up
            promote: false,
        };
        assert!(moves.contains(&rook_move));
    }

    #[test]
    fn promotion_on_last_rank() {
        let mut pos = Position::new();
        // Black Pawn about to reach last rank
        pos.set_sfen("4k/P4/5/5/K4 b - 1").unwrap();

        let from = Square::new(0, 1).unwrap();
        let to = Square::new(0, 0).unwrap();

        // Must promote
        let mv = Move::Normal { from, to, promote: true };
        pos.make_move(mv).unwrap();

        assert_eq!(Some(Piece { piece_type: PieceType::ProPawn, color: Color::Black }), pos.piece_at(to));
    }
}
