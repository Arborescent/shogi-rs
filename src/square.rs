use crate::Color;
use std::fmt;
use std::iter;
use std::marker::PhantomData;
use std::str::FromStr;

const ASCII_1: u8 = b'1';
const ASCII_LOWER_A: u8 = b'a';

/// Represents a position of each cell in a game board with dimensions W×H.
///
/// The board dimensions are specified as const generic parameters:
/// - `W`: Width (number of files), 1-26
/// - `H`: Height (number of ranks), 1-26
///
/// # Examples
///
/// ```
/// use shogi::Square;
///
/// let sq = Square::new(4, 4).unwrap();
/// assert_eq!("5e", sq.to_string());
/// ```
///
/// `Square` can be created by parsing a SFEN formatted string as well.
///
/// ```
/// use shogi::Square;
///
/// let sq = Square::from_sfen("5e").unwrap();
/// assert_eq!(4, sq.file());
/// assert_eq!(4, sq.rank());
/// ```
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Square<const W: u8, const H: u8> {
    inner: u8,
    _marker: PhantomData<()>,
}

/// Type alias for standard 9×9 Shogi board squares.
pub type StandardSquare = Square<9, 9>;

/// Type alias for 5×5 Mini Shogi board squares.
pub type MiniSquare = Square<5, 5>;

impl<const W: u8, const H: u8> Square<W, H> {
    /// The total number of squares on the board.
    pub const NUM_SQUARES: usize = (W as usize) * (H as usize);

    /// Creates a new instance of `Square`.
    ///
    /// `file` can take a value from 0 to W-1, while `rank` is from 0 to H-1.
    pub fn new(file: u8, rank: u8) -> Option<Self> {
        if file >= W || rank >= H {
            return None;
        }

        Some(Square {
            inner: file * H + rank,
            _marker: PhantomData,
        })
    }

    /// Creates a new instance of `Square` from SFEN formatted string.
    ///
    /// For boards with width ≤ 9, files are numbered 1-9.
    /// For wider boards, files use digits (for now, up to 26 files supported).
    /// Ranks use letters a-z (up to 26 ranks supported).
    pub fn from_sfen(s: &str) -> Option<Self> {
        let bytes: &[u8] = s.as_bytes();

        if bytes.len() < 2 {
            return None;
        }

        // Parse file (1-based number)
        let (file, rank_start) = if W <= 9 {
            // Single digit file
            if bytes.len() != 2 {
                return None;
            }
            let file_char = bytes[0];
            if file_char < ASCII_1 || file_char >= ASCII_1 + W {
                return None;
            }
            (file_char - ASCII_1, 1)
        } else {
            // Multi-digit file for wider boards
            let mut file_end = 0;
            while file_end < bytes.len() && bytes[file_end].is_ascii_digit() {
                file_end += 1;
            }
            if file_end == 0 || file_end >= bytes.len() {
                return None;
            }
            let file_str = std::str::from_utf8(&bytes[..file_end]).ok()?;
            let file_num: u8 = file_str.parse().ok()?;
            if file_num == 0 || file_num > W {
                return None;
            }
            (file_num - 1, file_end)
        };

        // Parse rank (letter a-z)
        if rank_start >= bytes.len() {
            return None;
        }
        let rank_char = bytes[rank_start];
        if rank_char < ASCII_LOWER_A || rank_char >= ASCII_LOWER_A + H {
            return None;
        }
        let rank = rank_char - ASCII_LOWER_A;

        // Ensure no extra characters
        if rank_start + 1 != bytes.len() {
            return None;
        }

        debug_assert!(
            file < W && rank < H,
            "{s} parsed as (file: {file}, rank: {rank})"
        );

        Some(Square {
            inner: file * H + rank,
            _marker: PhantomData,
        })
    }

    /// Creates a new instance of `Square` with the given index value.
    pub fn from_index(index: u8) -> Option<Self> {
        if index as usize >= Self::NUM_SQUARES {
            return None;
        }

        Some(Square {
            inner: index,
            _marker: PhantomData,
        })
    }

    /// Returns an iterator of all squares on the board.
    pub fn iter() -> SquareIter<W, H> {
        SquareIter {
            current: 0,
            _marker: PhantomData,
        }
    }

    /// Returns the file (column) of the square (0-indexed).
    pub fn file(self) -> u8 {
        self.inner / H
    }

    /// Returns the rank (row) of the square (0-indexed).
    pub fn rank(self) -> u8 {
        self.inner % H
    }

    /// Returns a new `Square` instance by moving the file and the rank values.
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::square::consts::*;
    ///
    /// let sq = SQ_2B;
    /// let shifted = sq.shift(2, 3).unwrap();
    ///
    /// assert_eq!(3, shifted.file());
    /// assert_eq!(4, shifted.rank());
    /// ```
    #[must_use]
    pub fn shift(self, df: i8, dr: i8) -> Option<Self> {
        let f = self.file() as i8 + df;
        let r = self.rank() as i8 + dr;

        if !(0..W as i8).contains(&f) || !(0..H as i8).contains(&r) {
            return None;
        }

        Some(Square {
            inner: (f as u8 * H + r as u8),
            _marker: PhantomData,
        })
    }

    /// Returns a relative rank as if the specified color is black.
    ///
    /// For black, returns the rank as-is (0 = closest to black's side).
    /// For white, returns H-1-rank (0 = closest to white's side).
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Color;
    /// use shogi::square::consts::*;
    ///
    /// let sq = SQ_1G;
    ///
    /// assert_eq!(6, sq.relative_rank(Color::Black));
    /// assert_eq!(2, sq.relative_rank(Color::White));
    /// ```
    pub fn relative_rank(self, c: Color) -> u8 {
        if c == Color::Black {
            self.rank()
        } else {
            H - 1 - self.rank()
        }
    }

    /// Converts the instance into the unique number for array indexing purpose.
    #[inline(always)]
    pub fn index(self) -> usize {
        self.inner as usize
    }

    /// Returns the width of the board.
    #[inline(always)]
    pub const fn width() -> u8 {
        W
    }

    /// Returns the height of the board.
    #[inline(always)]
    pub const fn height() -> u8 {
        H
    }

    /// Returns both file and rank as a tuple.
    ///
    /// This is a convenience method equivalent to `(sq.file(), sq.rank())`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq = Square::new(4, 3).unwrap();
    /// assert_eq!((4, 3), sq.coordinates());
    /// ```
    #[inline(always)]
    pub fn coordinates(self) -> (u8, u8) {
        (self.file(), self.rank())
    }

    /// Returns the SFEN string representation.
    ///
    /// This is equivalent to using the Display trait but as an explicit method.
    pub fn to_sfen(self) -> String {
        self.to_string()
    }

    /// Returns the file as a character for SFEN notation.
    ///
    /// For standard boards (width ≤ 9), returns '1'-'9'.
    /// For wider boards, returns the file number as a string.
    pub fn file_char(self) -> char {
        if W <= 9 {
            (self.file() + ASCII_1) as char
        } else {
            // For wider boards, this returns the first digit
            char::from_digit((self.file() + 1) as u32, 10).unwrap_or('?')
        }
    }

    /// Returns the rank as a character for SFEN notation.
    ///
    /// Returns 'a'-'i' for standard boards, extended for larger boards.
    pub fn rank_char(self) -> char {
        (self.rank() + ASCII_LOWER_A) as char
    }

    /// Returns the file in Japanese notation (full-width numeral).
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq = Square::new(6, 5).unwrap(); // 7f
    /// assert_eq!("７", sq.file_japanese());
    /// ```
    pub fn file_japanese(self) -> &'static str {
        match self.file() + 1 {
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

    /// Returns the rank in Japanese notation (kanji numeral).
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq = Square::new(6, 5).unwrap(); // 7f
    /// assert_eq!("六", sq.rank_japanese());
    /// ```
    pub fn rank_japanese(self) -> &'static str {
        match self.rank() {
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

    /// Returns the square in Japanese notation (e.g., "７六").
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq = Square::new(6, 5).unwrap(); // 7f
    /// assert_eq!("７六", sq.to_japanese());
    /// ```
    pub fn to_japanese(self) -> String {
        format!("{}{}", self.file_japanese(), self.rank_japanese())
    }

    /// Creates a square from Japanese notation string.
    ///
    /// Accepts strings like "７六" (full-width) or "7六" (mixed).
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq = Square::from_japanese("７六").unwrap();
    /// assert_eq!(6, sq.file());
    /// assert_eq!(5, sq.rank());
    /// ```
    pub fn from_japanese(s: &str) -> Option<Self> {
        let chars: Vec<char> = s.chars().collect();
        if chars.len() < 2 {
            return None;
        }

        // Parse file (first character - full-width or half-width digit)
        let file = match chars[0] {
            '１' | '1' => 0,
            '２' | '2' => 1,
            '３' | '3' => 2,
            '４' | '4' => 3,
            '５' | '5' => 4,
            '６' | '6' => 5,
            '７' | '7' => 6,
            '８' | '8' => 7,
            '９' | '9' => 8,
            _ => return None,
        };

        // Parse rank (second character - kanji)
        let rank = match chars[1] {
            '一' => 0,
            '二' => 1,
            '三' => 2,
            '四' => 3,
            '五' => 4,
            '六' => 5,
            '七' => 6,
            '八' => 7,
            '九' => 8,
            _ => return None,
        };

        if file >= W || rank >= H {
            return None;
        }

        Self::new(file, rank)
    }
}

impl<const W: u8, const H: u8> fmt::Display for Square<W, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        debug_assert!(
            self.file() < W && self.rank() < H,
            "trying to stringify an invalid square: {self:?}"
        );
        if W <= 9 {
            write!(
                f,
                "{}{}",
                (self.file() + ASCII_1) as char,
                (self.rank() + ASCII_LOWER_A) as char
            )
        } else {
            // For wider boards, use multi-digit file numbers
            write!(
                f,
                "{}{}",
                self.file() + 1,
                (self.rank() + ASCII_LOWER_A) as char
            )
        }
    }
}

/// Error type for parsing a square from SFEN notation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseSquareError;

impl fmt::Display for ParseSquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid square notation")
    }
}

impl std::error::Error for ParseSquareError {}

impl<const W: u8, const H: u8> FromStr for Square<W, H> {
    type Err = ParseSquareError;

    /// Parses a square from SFEN notation (e.g., "7g", "5e").
    ///
    /// # Examples
    ///
    /// ```
    /// use shogi::Square;
    ///
    /// let sq: Square = "7g".parse().unwrap();
    /// assert_eq!(sq.file(), 6);
    /// assert_eq!(sq.rank(), 6);
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_sfen(s).ok_or(ParseSquareError)
    }
}

/// Square constants for standard 9×9 Shogi boards.
pub mod consts {
    use super::StandardSquare;
    use std::marker::PhantomData;

    macro_rules! make_square {
        {0, $t:ident $($ts:ident)+} => {
            pub const $t: StandardSquare = StandardSquare { inner: 0, _marker: PhantomData };
            make_square!{1, $($ts)*}
        };
        {$n:expr, $t:ident $($ts:ident)+} => {
            pub const $t: StandardSquare = StandardSquare { inner: $n, _marker: PhantomData };
            make_square!{($n + 1), $($ts)*}
        };
        {$n:expr, $t:ident} => {
            pub const $t: StandardSquare = StandardSquare { inner: $n, _marker: PhantomData };
        };
    }

    make_square! {0, SQ_1A SQ_1B SQ_1C SQ_1D SQ_1E SQ_1F SQ_1G SQ_1H SQ_1I
    SQ_2A SQ_2B SQ_2C SQ_2D SQ_2E SQ_2F SQ_2G SQ_2H SQ_2I
    SQ_3A SQ_3B SQ_3C SQ_3D SQ_3E SQ_3F SQ_3G SQ_3H SQ_3I
    SQ_4A SQ_4B SQ_4C SQ_4D SQ_4E SQ_4F SQ_4G SQ_4H SQ_4I
    SQ_5A SQ_5B SQ_5C SQ_5D SQ_5E SQ_5F SQ_5G SQ_5H SQ_5I
    SQ_6A SQ_6B SQ_6C SQ_6D SQ_6E SQ_6F SQ_6G SQ_6H SQ_6I
    SQ_7A SQ_7B SQ_7C SQ_7D SQ_7E SQ_7F SQ_7G SQ_7H SQ_7I
    SQ_8A SQ_8B SQ_8C SQ_8D SQ_8E SQ_8F SQ_8G SQ_8H SQ_8I
    SQ_9A SQ_9B SQ_9C SQ_9D SQ_9E SQ_9F SQ_9G SQ_9H SQ_9I}
}

/// This struct is created by the [`iter`] method on [`Square`].
///
/// [`iter`]: ./struct.Square.html#method.iter
/// [`Square`]: struct.Square.html
pub struct SquareIter<const W: u8, const H: u8> {
    current: u8,
    _marker: PhantomData<()>,
}

impl<const W: u8, const H: u8> iter::Iterator for SquareIter<W, H> {
    type Item = Square<W, H>;

    fn next(&mut self) -> Option<Self::Item> {
        let cur = self.current;
        let num_squares = (W as u8) * (H as u8);

        if cur >= num_squares {
            return None;
        }

        self.current += 1;

        Some(Square {
            inner: cur,
            _marker: PhantomData,
        })
    }
}

/// Type alias for standard 9×9 Shogi board square iterator.
pub type StandardSquareIter = SquareIter<9, 9>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_standard() {
        for file in 0..9 {
            for rank in 0..9 {
                let sq = StandardSquare::new(file, rank).unwrap();
                assert_eq!(file, sq.file());
                assert_eq!(rank, sq.rank());
            }
        }

        assert_eq!(None, StandardSquare::new(10, 0));
        assert_eq!(None, StandardSquare::new(0, 10));
        assert_eq!(None, StandardSquare::new(10, 10));
    }

    #[test]
    fn new_mini() {
        for file in 0..5 {
            for rank in 0..5 {
                let sq = MiniSquare::new(file, rank).unwrap();
                assert_eq!(file, sq.file());
                assert_eq!(rank, sq.rank());
            }
        }

        assert_eq!(None, MiniSquare::new(5, 0));
        assert_eq!(None, MiniSquare::new(0, 5));
    }

    #[test]
    fn from_sfen() {
        let ok_cases = [
            ("9a", 8, 0),
            ("1a", 0, 0),
            ("5e", 4, 4),
            ("9i", 8, 8),
            ("1i", 0, 8),
        ];
        let ng_cases = ["", "9j", "_a", "a9", "9 ", " a", "9", "foo"];

        for case in ok_cases.iter() {
            let sq = StandardSquare::from_sfen(case.0);
            assert!(sq.is_some());
            assert_eq!(case.1, sq.unwrap().file());
            assert_eq!(case.2, sq.unwrap().rank());
        }

        for case in ng_cases.iter() {
            assert!(
                StandardSquare::from_sfen(case).is_none(),
                "{case} should cause an error"
            );
        }
    }

    #[test]
    fn from_sfen_mini() {
        // Mini shogi uses files 1-5 and ranks a-e
        let ok_cases = [("1a", 0, 0), ("5e", 4, 4), ("3c", 2, 2)];
        let ng_cases = ["6a", "1f", "9i"];

        for case in ok_cases.iter() {
            let sq = MiniSquare::from_sfen(case.0);
            assert!(sq.is_some(), "{} should be valid", case.0);
            assert_eq!(case.1, sq.unwrap().file());
            assert_eq!(case.2, sq.unwrap().rank());
        }

        for case in ng_cases.iter() {
            assert!(
                MiniSquare::from_sfen(case).is_none(),
                "{case} should cause an error"
            );
        }
    }

    #[test]
    fn from_index() {
        for i in 0..81 {
            assert!(StandardSquare::from_index(i).is_some());
        }

        assert!(StandardSquare::from_index(82).is_none());

        // Mini shogi has 25 squares
        for i in 0..25 {
            assert!(MiniSquare::from_index(i).is_some());
        }
        assert!(MiniSquare::from_index(25).is_none());
    }

    #[test]
    fn to_sfen() {
        let cases = [
            ("9a", 8, 0),
            ("1a", 0, 0),
            ("5e", 4, 4),
            ("9i", 8, 8),
            ("1i", 0, 8),
        ];

        for case in cases.iter() {
            let sq = StandardSquare::new(case.1, case.2).unwrap();
            assert_eq!(case.0, sq.to_string());
        }
    }

    #[test]
    fn shift() {
        let sq = consts::SQ_5E;

        let ok_cases = [
            (-4, -4, 0, 0),
            (-4, 0, 0, 4),
            (0, -4, 4, 0),
            (0, 0, 4, 4),
            (4, 0, 8, 4),
            (0, 4, 4, 8),
            (4, 4, 8, 8),
        ];

        let ng_cases = [(-5, -4), (-4, -5), (5, 0), (0, 5)];

        for case in ok_cases.iter() {
            let shifted = sq.shift(case.0, case.1).unwrap();
            assert_eq!(case.2, shifted.file());
            assert_eq!(case.3, shifted.rank());
        }

        for case in ng_cases.iter() {
            assert!(sq.shift(case.0, case.1).is_none());
        }
    }

    #[test]
    fn relative_rank() {
        let cases = [
            (0, 0, 0, 8),
            (0, 1, 1, 7),
            (0, 2, 2, 6),
            (0, 3, 3, 5),
            (0, 4, 4, 4),
            (0, 5, 5, 3),
            (0, 6, 6, 2),
            (0, 7, 7, 1),
            (0, 8, 8, 0),
        ];

        for case in cases.iter() {
            let sq = StandardSquare::new(case.0, case.1).unwrap();
            assert_eq!(case.2, sq.relative_rank(Color::Black));
            assert_eq!(case.3, sq.relative_rank(Color::White));
        }
    }

    #[test]
    fn relative_rank_mini() {
        // Mini shogi: 5x5 board
        let cases = [(0, 0, 0, 4), (0, 2, 2, 2), (0, 4, 4, 0)];

        for case in cases.iter() {
            let sq = MiniSquare::new(case.0, case.1).unwrap();
            assert_eq!(case.2, sq.relative_rank(Color::Black));
            assert_eq!(case.3, sq.relative_rank(Color::White));
        }
    }

    #[test]
    fn consts_standard() {
        for (i, sq) in StandardSquare::iter().enumerate() {
            assert_eq!((i / 9) as u8, sq.file());
            assert_eq!((i % 9) as u8, sq.rank());
        }
    }

    #[test]
    fn num_squares() {
        assert_eq!(81, StandardSquare::NUM_SQUARES);
        assert_eq!(25, MiniSquare::NUM_SQUARES);
    }

    #[test]
    fn coordinates() {
        let sq = StandardSquare::new(4, 3).unwrap();
        assert_eq!((4, 3), sq.coordinates());

        let sq = StandardSquare::new(8, 8).unwrap();
        assert_eq!((8, 8), sq.coordinates());
    }

    #[test]
    fn sfen_chars() {
        let sq = StandardSquare::new(4, 3).unwrap(); // 5d
        assert_eq!('5', sq.file_char());
        assert_eq!('d', sq.rank_char());

        let sq = StandardSquare::new(0, 0).unwrap(); // 1a
        assert_eq!('1', sq.file_char());
        assert_eq!('a', sq.rank_char());

        let sq = StandardSquare::new(8, 8).unwrap(); // 9i
        assert_eq!('9', sq.file_char());
        assert_eq!('i', sq.rank_char());
    }

    #[test]
    fn japanese_notation() {
        // 7f -> ７六
        let sq = StandardSquare::new(6, 5).unwrap();
        assert_eq!("７", sq.file_japanese());
        assert_eq!("六", sq.rank_japanese());
        assert_eq!("７六", sq.to_japanese());

        // 1a -> １一
        let sq = StandardSquare::new(0, 0).unwrap();
        assert_eq!("１一", sq.to_japanese());

        // 9i -> ９九
        let sq = StandardSquare::new(8, 8).unwrap();
        assert_eq!("９九", sq.to_japanese());
    }

    #[test]
    fn from_japanese_notation() {
        // Full-width numerals
        let sq = StandardSquare::from_japanese("７六").unwrap();
        assert_eq!(6, sq.file());
        assert_eq!(5, sq.rank());

        // Half-width file numeral
        let sq = StandardSquare::from_japanese("7六").unwrap();
        assert_eq!(6, sq.file());
        assert_eq!(5, sq.rank());

        // Invalid inputs
        assert!(StandardSquare::from_japanese("").is_none());
        assert!(StandardSquare::from_japanese("７").is_none());
        assert!(StandardSquare::from_japanese("7f").is_none()); // SFEN, not Japanese
        assert!(StandardSquare::from_japanese("０一").is_none()); // File 0 invalid
    }

    #[test]
    fn roundtrip_japanese() {
        for file in 0..9 {
            for rank in 0..9 {
                let sq = StandardSquare::new(file, rank).unwrap();
                let jp = sq.to_japanese();
                let parsed = StandardSquare::from_japanese(&jp).unwrap();
                assert_eq!(sq, parsed);
            }
        }
    }
}
