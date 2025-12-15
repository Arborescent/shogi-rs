use std::fmt;
use std::iter;
use std::marker::PhantomData;
use std::ops;

use super::square::Square;

/// Represents a board state in which each square takes two possible values, filled or empty.
///
/// The board dimensions are specified as const generic parameters:
/// - `W`: Width (number of files)
/// - `H`: Height (number of ranks)
///
/// `Bitboard` implements [PEXT Bitboard](https://www.chessprogramming.org/BMI2#PEXTBitboards) which relies on [BMI2 instruction set](https://www.chessprogramming.org/BMI2).
/// For environments which do not support BMI2, it will use software fallback methods. Thanks to [bitintr](https://github.com/gnzlbg/bitintr) crate.
///
/// # Examples
///
/// ```
/// use shogi::Bitboard;
/// use shogi::square::consts::*;
///
/// let mut bb = Bitboard::empty();
/// bb ^= SQ_1A;
/// bb |= SQ_9I;
///
/// assert_eq!(2, bb.count());
/// assert_eq!(1, bb.filter(|sq| sq.file() == 0).count());
/// ```
#[derive(Debug, Default, Clone, Copy)]
pub struct Bitboard<const W: u8, const H: u8> {
    pub(crate) p: [u64; 2],
    _marker: PhantomData<()>,
}

/// Type alias for standard 9×9 Shogi bitboard.
pub type StandardBitboard = Bitboard<9, 9>;

/// Type alias for 5×5 Mini Shogi bitboard.
pub type MiniBitboard = Bitboard<5, 5>;

/////////////////////////////////////////////////////////////////////////////
// Type implementation
/////////////////////////////////////////////////////////////////////////////

impl<const W: u8, const H: u8> Bitboard<W, H> {
    /// The total number of squares on the board.
    pub const NUM_SQUARES: usize = (W as usize) * (H as usize);

    /// Returns an empty instance of `Bitboard`.
    #[inline(always)]
    pub fn empty() -> Self {
        Bitboard {
            p: [0, 0],
            _marker: PhantomData,
        }
    }

    /// Creates a bitboard with a single square set.
    #[inline(always)]
    pub fn from_square(sq: Square<W, H>) -> Self {
        let idx = sq.index();
        let mut bb = Self::empty();
        if idx < 63 {
            bb.p[0] = 1u64 << idx;
        } else {
            bb.p[1] = 1u64 << (idx - 63);
        }
        bb
    }

    /// Checks if any of its squares is filled.
    #[inline(always)]
    pub fn is_any(&self) -> bool {
        (self.p[0] | self.p[1]) != 0
    }

    /// Checks if all of its squares are empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        (self.p[0] | self.p[1]) == 0
    }

    /// Checks if the given square is set.
    #[inline(always)]
    pub fn contains(&self, sq: Square<W, H>) -> bool {
        let idx = sq.index();
        if idx < 63 {
            (self.p[0] & (1u64 << idx)) != 0
        } else {
            (self.p[1] & (1u64 << (idx - 63))) != 0
        }
    }

    /// Sets the given square.
    #[inline(always)]
    pub fn set_at(&mut self, sq: Square<W, H>) {
        let idx = sq.index();
        if idx < 63 {
            self.p[0] |= 1u64 << idx;
        } else {
            self.p[1] |= 1u64 << (idx - 63);
        }
    }

    /// Sets the given square as empty.
    #[inline(always)]
    pub fn clear_at(&mut self, sq: Square<W, H>) {
        let idx = sq.index();
        if idx < 63 {
            self.p[0] &= !(1u64 << idx);
        } else {
            self.p[1] &= !(1u64 << (idx - 63));
        }
    }

    /// Returns the number of squares filled.
    #[inline(always)]
    pub fn count(&self) -> u32 {
        self.p[0].count_ones() + self.p[1].count_ones()
    }

    /// Sets the first filled square as empty and returns that square.
    ///
    /// This method expects the bitboard not being empty.
    #[inline(always)]
    pub fn pop(&mut self) -> Square<W, H> {
        if self.p[0] != 0 {
            let sq = Square::from_index(self.p[0].trailing_zeros() as u8).unwrap();
            self.p[0] &= self.p[0] - 1;
            sq
        } else {
            let sq = Square::from_index(self.p[1].trailing_zeros() as u8 + 63).unwrap();
            self.p[1] &= self.p[1] - 1;
            sq
        }
    }

    /// Merges both u64 parts into a single value (only valid for boards <= 64 squares).
    #[inline(always)]
    pub(crate) fn merge(&self) -> u64 {
        self.p[0] | self.p[1]
    }

    /// Creates a bitboard from raw parts.
    #[inline(always)]
    pub(crate) const fn from_parts(p0: u64, p1: u64) -> Self {
        Bitboard {
            p: [p0, p1],
            _marker: PhantomData,
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
// Operator implementations
/////////////////////////////////////////////////////////////////////////////

impl<'a, const W: u8, const H: u8> ops::Not for &'a Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn not(self) -> Bitboard<W, H> {
        Bitboard {
            p: [!self.p[0], !self.p[1]],
            _marker: PhantomData,
        }
    }
}

impl<'a, 'b, const W: u8, const H: u8> ops::BitAnd<&'a Bitboard<W, H>> for &'b Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitand(self, rhs: &'a Bitboard<W, H>) -> Bitboard<W, H> {
        Bitboard {
            p: [self.p[0] & rhs.p[0], self.p[1] & rhs.p[1]],
            _marker: PhantomData,
        }
    }
}

impl<'a, const W: u8, const H: u8> ops::BitAndAssign<&'a Bitboard<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: &'a Bitboard<W, H>) {
        self.p[0] &= rhs.p[0];
        self.p[1] &= rhs.p[1];
    }
}

impl<'a, 'b, const W: u8, const H: u8> ops::BitOr<&'a Bitboard<W, H>> for &'b Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitor(self, rhs: &'a Bitboard<W, H>) -> Bitboard<W, H> {
        Bitboard {
            p: [self.p[0] | rhs.p[0], self.p[1] | rhs.p[1]],
            _marker: PhantomData,
        }
    }
}

impl<'a, const W: u8, const H: u8> ops::BitOrAssign<&'a Bitboard<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: &'a Bitboard<W, H>) {
        self.p[0] |= rhs.p[0];
        self.p[1] |= rhs.p[1];
    }
}

impl<'a, 'b, const W: u8, const H: u8> ops::BitXor<&'a Bitboard<W, H>> for &'b Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitxor(self, rhs: &'a Bitboard<W, H>) -> Bitboard<W, H> {
        Bitboard {
            p: [self.p[0] ^ rhs.p[0], self.p[1] ^ rhs.p[1]],
            _marker: PhantomData,
        }
    }
}

impl<'a, const W: u8, const H: u8> ops::BitXorAssign<&'a Bitboard<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: &'a Bitboard<W, H>) {
        self.p[0] ^= rhs.p[0];
        self.p[1] ^= rhs.p[1];
    }
}

impl<'a, const W: u8, const H: u8> ops::BitAnd<Square<W, H>> for &'a Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitand(self, rhs: Square<W, H>) -> Bitboard<W, H> {
        self & &Bitboard::from_square(rhs)
    }
}

impl<const W: u8, const H: u8> ops::BitAndAssign<Square<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: Square<W, H>) {
        *self &= &Bitboard::from_square(rhs)
    }
}

impl<'a, const W: u8, const H: u8> ops::BitOr<Square<W, H>> for &'a Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitor(self, rhs: Square<W, H>) -> Bitboard<W, H> {
        self | &Bitboard::from_square(rhs)
    }
}

impl<const W: u8, const H: u8> ops::BitOrAssign<Square<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: Square<W, H>) {
        *self |= &Bitboard::from_square(rhs)
    }
}

impl<'a, const W: u8, const H: u8> ops::BitXor<Square<W, H>> for &'a Bitboard<W, H> {
    type Output = Bitboard<W, H>;

    #[inline(always)]
    fn bitxor(self, rhs: Square<W, H>) -> Bitboard<W, H> {
        self ^ &Bitboard::from_square(rhs)
    }
}

impl<const W: u8, const H: u8> ops::BitXorAssign<Square<W, H>> for Bitboard<W, H> {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: Square<W, H>) {
        *self ^= &Bitboard::from_square(rhs)
    }
}

/////////////////////////////////////////////////////////////////////////////
// Trait implementation
/////////////////////////////////////////////////////////////////////////////

impl<const W: u8, const H: u8> fmt::Display for Bitboard<W, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Print file header
        write!(f, "  ")?;
        for file in (0..W).rev() {
            write!(f, " {} ", file + 1)?;
        }
        writeln!(f)?;

        // Print separator
        write!(f, "+")?;
        for _ in 0..W {
            write!(f, "---+")?;
        }
        writeln!(f)?;

        for rank in 0..H {
            write!(f, "|")?;
            for file in (0..W).rev() {
                let sq = Square::new(file, rank).unwrap();
                write!(f, " {} |", if self.contains(sq) { "X" } else { " " })?;
            }
            writeln!(f, " {}", (b'a' + rank) as char)?;
            write!(f, "+")?;
            for _ in 0..W {
                write!(f, "---+")?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl<const W: u8, const H: u8> iter::Iterator for Bitboard<W, H> {
    type Item = Square<W, H>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_any() {
            Some(self.pop())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::square::{MiniSquare, StandardSquare};

    #[test]
    fn empty_bitboard() {
        let bb: StandardBitboard = Bitboard::empty();
        assert!(bb.is_empty());
        assert!(!bb.is_any());
        assert_eq!(0, bb.count());
    }

    #[test]
    fn from_square() {
        let sq = StandardSquare::new(4, 4).unwrap();
        let bb = StandardBitboard::from_square(sq);
        assert!(bb.is_any());
        assert_eq!(1, bb.count());
        assert!(bb.contains(sq));
    }

    #[test]
    fn set_and_clear() {
        let mut bb: StandardBitboard = Bitboard::empty();
        let sq = StandardSquare::new(0, 0).unwrap();

        bb.set_at(sq);
        assert!(bb.contains(sq));
        assert_eq!(1, bb.count());

        bb.clear_at(sq);
        assert!(!bb.contains(sq));
        assert_eq!(0, bb.count());
    }

    #[test]
    fn bitwise_operations() {
        let sq1 = StandardSquare::new(0, 0).unwrap();
        let sq2 = StandardSquare::new(1, 1).unwrap();

        let bb1 = StandardBitboard::from_square(sq1);
        let bb2 = StandardBitboard::from_square(sq2);

        let or_result = &bb1 | &bb2;
        assert_eq!(2, or_result.count());

        let and_result = &bb1 & &bb2;
        assert_eq!(0, and_result.count());
    }

    #[test]
    fn mini_board() {
        let mut bb: MiniBitboard = Bitboard::empty();
        let sq = MiniSquare::new(2, 2).unwrap();

        bb.set_at(sq);
        assert!(bb.contains(sq));
        assert_eq!(1, bb.count());
    }

    #[test]
    fn iterator() {
        let sq1 = StandardSquare::new(0, 0).unwrap();
        let sq2 = StandardSquare::new(1, 1).unwrap();

        let mut bb: StandardBitboard = Bitboard::empty();
        bb.set_at(sq1);
        bb.set_at(sq2);

        let squares: Vec<_> = bb.collect();
        assert_eq!(2, squares.len());
    }
}
