use std::time::Duration;

use super::color::Color;

/// Represents various time controls.
///
/// Currently
/// [Byo-yomi](https://en.wikipedia.org/wiki/Time_control#Byo-yomi) and
/// [Fischer Clock](https://en.wikipedia.org/wiki/Time_control#Increment_and_delay_methods)
/// are supported.
///
/// # Examples
///
/// ```
/// use std::time::Duration;
/// use shogi::{Color, TimeControl};
///
/// let mut byoyomi = TimeControl::Byoyomi{
///     black_time: Duration::from_secs(10),
///     white_time: Duration::from_secs(10),
///     byoyomi: Duration::from_secs(5)
/// };
///
/// // Black uses 8 seconds of main time
/// byoyomi.consume(Color::Black, Duration::from_secs(8));
/// assert_eq!(Duration::from_secs(2), byoyomi.black_time());
///
/// // Black uses remaining 2 seconds of main time
/// byoyomi.consume(Color::Black, Duration::from_secs(2));
/// assert_eq!(Duration::from_secs(0), byoyomi.black_time());
///
/// // After the move, reset byoyomi to give full period for next move
/// byoyomi.reset_byoyomi(Color::Black);
/// assert_eq!(Duration::from_secs(5), byoyomi.black_time());
/// ```
///
/// ```
/// use std::time::Duration;
/// use shogi::{Color, TimeControl};
///
/// let mut fischer_clock = TimeControl::FischerClock{
///     black_time: Duration::from_secs(10),
///     white_time: Duration::from_secs(10),
///     black_inc: Duration::from_secs(1),
///     white_inc: Duration::from_secs(1)
/// };
///
/// // White player gets additional 1 second after the black move.
/// fischer_clock.consume(Color::Black, Duration::from_secs(3));
/// assert_eq!(Duration::from_secs(8), fischer_clock.black_time());
/// assert_eq!(Duration::from_secs(10), fischer_clock.white_time());
/// ```
#[derive(Debug, Clone, Copy)]
pub enum TimeControl {
    /// Traditional per-move byoyomi: when main time is exhausted, the player enters
    /// byoyomi mode where they get a fixed amount of time per move. As long as they
    /// move within this time, they get the full byoyomi for their next move.
    /// Call `reset_byoyomi` after each successful move to reset the byoyomi period.
    Byoyomi {
        black_time: Duration,
        white_time: Duration,
        byoyomi: Duration,
    },
    FischerClock {
        black_time: Duration,
        white_time: Duration,
        black_inc: Duration,
        white_inc: Duration,
    },
}

impl TimeControl {
    /// Returns the current remaining time for the black player.
    pub fn black_time(&self) -> Duration {
        match *self {
            TimeControl::Byoyomi { black_time, .. } => black_time,
            TimeControl::FischerClock { black_time, .. } => black_time,
        }
    }

    /// Returns the current remaining time for the white player.
    pub fn white_time(&self) -> Duration {
        match *self {
            TimeControl::Byoyomi { white_time, .. } => white_time,
            TimeControl::FischerClock { white_time, .. } => white_time,
        }
    }

    /// Returns the byoyomi period (only for Byoyomi variant).
    pub fn byoyomi(&self) -> Option<Duration> {
        match *self {
            TimeControl::Byoyomi { byoyomi, .. } => Some(byoyomi),
            _ => None,
        }
    }

    /// Resets the byoyomi period for the player who just moved.
    ///
    /// For `Byoyomi`: if the player's remaining time is less than the byoyomi period
    /// (meaning they're in byoyomi mode), this resets their time to the full byoyomi period.
    ///
    /// This method should be called after each successful move when using `Byoyomi`.
    /// For other time control variants, this method has no effect.
    pub fn reset_byoyomi(&mut self, c: Color) {
        if let TimeControl::Byoyomi {
            ref mut black_time,
            ref mut white_time,
            byoyomi,
        } = self
        {
            let target_time = if c == Color::Black {
                black_time
            } else {
                white_time
            };

            // If in byoyomi mode (time < byoyomi), reset to full byoyomi
            if *target_time < *byoyomi {
                *target_time = *byoyomi;
            }
        }
    }

    /// Updates the current remaining time after consuming the given amount of time for the given player.
    ///
    /// Returns false if the given player runs out of time, true otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::time::Duration;
    /// use shogi::{Color, TimeControl};
    ///
    /// let mut byoyomi = TimeControl::Byoyomi{
    ///     black_time: Duration::from_secs(10),
    ///     white_time: Duration::from_secs(10),
    ///     byoyomi: Duration::from_secs(5)
    /// };
    ///
    /// // Use 8 seconds - within time
    /// assert!(byoyomi.consume(Color::Black, Duration::from_secs(8)));
    /// // Use 3 seconds - exceeds remaining 2 seconds, time out
    /// assert!(!byoyomi.consume(Color::Black, Duration::from_secs(3)));
    /// ```
    pub fn consume(&mut self, c: Color, d: Duration) -> bool {
        match *self {
            TimeControl::Byoyomi {
                ref mut black_time,
                ref mut white_time,
                ref byoyomi,
            } => {
                let target_time = if c == Color::Black {
                    black_time
                } else {
                    white_time
                };

                // Time out if consumed time exceeds remaining time
                // (Note: when in byoyomi mode, target_time is the byoyomi period)
                if d > *target_time {
                    return false;
                }
                *target_time -= d;
                // After a successful move, caller should call reset_byoyomi() to give
                // the player full byoyomi for their next move if they're in byoyomi mode.
                let _ = byoyomi; // Suppress unused warning
            }
            TimeControl::FischerClock {
                ref mut black_time,
                ref mut white_time,
                ref black_inc,
                ref white_inc,
            } => {
                let (stm_time, inc_time) = if c == Color::Black {
                    (black_time, black_inc)
                } else {
                    (white_time, white_inc)
                };

                *stm_time += *inc_time;
                if d > *stm_time {
                    return false;
                }
                *stm_time -= d;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn consume_byoyomi() {
        // Test basic byoyomi consumption
        let mut tc = TimeControl::Byoyomi {
            black_time: Duration::from_secs(300), // 5 minutes main time
            white_time: Duration::from_secs(300),
            byoyomi: Duration::from_secs(30), // 30 seconds per move in byoyomi
        };

        // Black uses 200 seconds (within main time)
        assert!(tc.consume(Color::Black, Duration::from_secs(200)));
        assert_eq!(Duration::from_secs(100), tc.black_time());

        // Black uses another 100 seconds, exhausting main time
        assert!(tc.consume(Color::Black, Duration::from_secs(100)));
        assert_eq!(Duration::from_secs(0), tc.black_time());

        // Black is now in byoyomi mode - reset_byoyomi should give them 30 seconds
        tc.reset_byoyomi(Color::Black);
        assert_eq!(Duration::from_secs(30), tc.black_time());

        // Black uses 25 seconds (within byoyomi)
        assert!(tc.consume(Color::Black, Duration::from_secs(25)));
        assert_eq!(Duration::from_secs(5), tc.black_time());

        // Reset byoyomi after the move - should get full 30 seconds again
        tc.reset_byoyomi(Color::Black);
        assert_eq!(Duration::from_secs(30), tc.black_time());

        // If black exceeds byoyomi, they lose on time
        assert!(!tc.consume(Color::Black, Duration::from_secs(31)));

        // White still has full main time
        assert_eq!(Duration::from_secs(300), tc.white_time());
    }

    #[test]
    fn byoyomi_accessor() {
        let tc = TimeControl::Byoyomi {
            black_time: Duration::from_secs(300),
            white_time: Duration::from_secs(300),
            byoyomi: Duration::from_secs(30),
        };
        assert_eq!(Some(Duration::from_secs(30)), tc.byoyomi());

        let tc_fischer = TimeControl::FischerClock {
            black_time: Duration::from_secs(300),
            white_time: Duration::from_secs(300),
            black_inc: Duration::from_secs(5),
            white_inc: Duration::from_secs(5),
        };
        assert_eq!(None, tc_fischer.byoyomi());
    }

    #[test]
    fn consume_fischer() {
        // black_time, white_time, black_inc, white_inc, consume, remaining_black, remaining_white
        let ok_cases = [
            (50, 50, 5, 5, 10, 45, 50),
            (50, 50, 5, 5, 50, 5, 50),
            (50, 50, 0, 0, 50, 0, 50),
        ];

        // black_time, white_time, black_inc, white_inc, consume
        let ng_cases = [(50, 50, 5, 5, 56)];

        for case in ok_cases.iter() {
            let mut t = TimeControl::FischerClock {
                black_time: Duration::from_secs(case.0),
                white_time: Duration::from_secs(case.1),
                black_inc: Duration::from_secs(case.2),
                white_inc: Duration::from_secs(case.3),
            };

            assert!(t.consume(Color::Black, Duration::from_secs(case.4)));
            assert_eq!(Duration::from_secs(case.5), t.black_time());
            assert_eq!(Duration::from_secs(case.6), t.white_time());
        }

        for case in ng_cases.iter() {
            let mut t = TimeControl::FischerClock {
                black_time: Duration::from_secs(case.0),
                white_time: Duration::from_secs(case.1),
                black_inc: Duration::from_secs(case.2),
                white_inc: Duration::from_secs(case.3),
            };

            assert!(!t.consume(Color::Black, Duration::from_secs(case.4)));
            assert!(!t.consume(Color::White, Duration::from_secs(case.4)));
        }
    }
}
