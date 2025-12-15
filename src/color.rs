//! Color type representing each player side.
//!
//! This module re-exports from `core::color` for backward compatibility.

pub use crate::core::color::{Color, ColorIter};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flip() {
        assert_eq!(Color::White, Color::Black.flip());
        assert_eq!(Color::Black, Color::White.flip());
    }
}
