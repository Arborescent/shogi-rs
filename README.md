# shogi-rs

[![Github Actions](https://github.com/nozaq/shogi-rs/workflows/build/badge.svg)](https://github.com/nozaq/shogi-rs/actions?workflow=build)
[![Coverage Status](https://coveralls.io/repos/github/nozaq/shogi-rs/badge.svg)](https://coveralls.io/github/nozaq/shogi-rs)
[![crates.io](https://img.shields.io/crates/v/shogi.svg)](https://crates.io/crates/shogi)
[![docs.rs](https://docs.rs/shogi/badge.svg)](https://docs.rs/shogi)

A Bitboard-based shogi library in Rust. Board representation, move generation/validation and time control utilities.

> **Note**: This is a fork of [nozaq/shogi-rs](https://github.com/nozaq/shogi-rs) with additional features including Mini Shogi support, game state detection, and notation formatting.

[Documentation](https://docs.rs/shogi)

## Features

- **Bitboard-based board representation** for efficient move generation
- **Move generation and validation** with full shogi rules enforcement
- **SFEN format support** with idiomatic `FromStr`/`Display` traits (USI protocol compatible)
- **Game state detection**: checkmate, stalemate, repetition, and impasse (jishogi)
- **Multiple notation formats**: Japanese (☗７六歩), Hosking (P76), Hodges (P7f)
- **Time control utilities**: Fischer, Byoyomi, and combination modes
- **Generic board sizes** via const generics for variant support

## Supported Variants

| Variant | Board | Description |
|---------|-------|-------------|
| **Standard Shogi** | 9×9 | Full implementation with all 14 piece types |
| **Mini Shogi** | 5×5 | Variant with 6 piece types (no Knight/Lance) |

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
shogi = { git = "https://github.com/Arborescent/shogi-rs.git" }
```

Or from crates.io (may not include latest features):

```toml
[dependencies]
shogi = "0.12"
```

## Quick Start

```rust
use shogi::{Move, Position};
use shogi::bitboard::Factory as BBFactory;
use shogi::square::consts::*;

// Initialize bitboard attack tables (required once at startup)
BBFactory::init();

let mut pos = Position::new();

// Set position from SFEN string
pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();

// Make moves programmatically
let m = Move::Normal { from: SQ_7G, to: SQ_7F, promote: false };
pos.make_move(m).unwrap();

// Or parse moves from SFEN notation (idiomatic FromStr)
let m: Move = "7c7d".parse().unwrap();
pos.make_move(m).unwrap();

// Convert position back to SFEN
assert_eq!(
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 moves 7g7f 7c7d",
    pos.to_sfen()
);
```

## Examples

### Move Generation

```rust
use shogi::{Move, Position};
use shogi::bitboard::Factory as BBFactory;

BBFactory::init();
let mut pos = Position::new();
pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();

// Get all legal moves for the current player
let legal_moves = pos.legal_moves();
println!("Found {} legal moves", legal_moves.len());

// Check if a specific move is legal
let m: Move = "7g7f".parse().unwrap();
assert!(pos.is_legal_move(m));
```

### Checkmate Detection

```rust
use shogi::{Position, GameStatus};
use shogi::bitboard::Factory as BBFactory;

BBFactory::init();
let mut pos = Position::new();

// Set up a position and check game status
pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();

match pos.game_status() {
    GameStatus::InProgress => println!("Game is ongoing"),
    GameStatus::Checkmate(winner) => println!("{:?} wins by checkmate!", winner),
    GameStatus::Stalemate => println!("Draw by stalemate"),
    GameStatus::Repetition => println!("Draw by repetition"),
}

// Direct checks
if pos.is_checkmate() {
    println!("Checkmate!");
} else if pos.in_check() {
    println!("Check!");
}
```

### Move Notation Formatting

```rust
use shogi::{Position, NotationFormat};
use shogi::bitboard::Factory as BBFactory;

BBFactory::init();
let mut pos = Position::new();
pos.set_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").unwrap();

let m = shogi::Move::from_sfen("7g7f").unwrap();
pos.make_move(m).unwrap();

// Get the last move record
let record = pos.history().last().unwrap();

// Format in different notations
println!("{}", record.to_japanese());           // ☗７六歩
println!("{}", record.to_hosking());            // P76
println!("{}", record.to_hodges());             // P7f
println!("{}", record.to_notation(NotationFormat::Japanese));
```

### SFEN Parsing and Manipulation

```rust
use shogi::{Sfen, Move, Square, Color};

// Parse SFEN strings idiomatically using FromStr
let sfen: Sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
    .parse()
    .unwrap();

// Access SFEN components
assert_eq!(sfen.side_to_move(), Color::Black);
assert_eq!(sfen.ply(), 1);
assert_eq!(sfen.hand(), "-");

// Parse moves and squares
let mv: Move = "7g7f".parse().unwrap();
let sq: Square = "5e".parse().unwrap();

// Convert back to string using Display
println!("{}", sfen);  // outputs the SFEN string

// Use SFEN utilities for manipulation
use shogi::sfen::{mirror_sfen, reset_move_number};

// Mirror position for opponent's perspective (rotates 180°, swaps colors)
let mirrored = mirror_sfen("9/9/9/9/4k4/9/9/9/4K4 b - 1");

// Reset move number (useful for puzzles)
let reset = reset_move_number("lnsgkgsnl/... b - 71");
```

### Mini Shogi (5×5 Variant)

```rust
use shogi::minishogi::{Position, Move, STARTING_SFEN};

let mut pos = Position::new();

// Mini Shogi starting position: rbsgk/4p/5/P4/KGSBR b - 1
pos.set_sfen(STARTING_SFEN).unwrap();

// Make a move
let m = Move::from_sfen("1e2e").unwrap();  // Rook forward
pos.make_move(m).unwrap();

// Check game status
if pos.is_checkmate() {
    println!("Checkmate!");
}
```

### Time Control

```rust
use shogi::TimeControl;
use std::time::Duration;

// Fischer time control: 10 minutes + 30 seconds increment
let mut tc = TimeControl::new(
    Duration::from_secs(600),  // main time
    Duration::from_secs(30),   // increment
    Duration::ZERO,            // no byoyomi
);

// Consume time for a move
tc.consume(Duration::from_secs(45));
println!("Remaining: {:?}", tc.remaining());

// Byoyomi time control: 30 seconds per move after main time
let mut tc = TimeControl::new(
    Duration::from_secs(0),    // no main time
    Duration::ZERO,            // no increment
    Duration::from_secs(30),   // 30 second byoyomi
);
```

## Module Organization

```
shogi/
├── core/           # Generic types shared across variants
│   ├── bitboard/   # Bitboard<W,H> for any board size
│   ├── square/     # Square<W,H> for any board size
│   ├── color/      # Color enum (Black/White)
│   └── error/      # Error types
├── traits/         # Trait definitions for variant-agnostic code
│   ├── piece/      # PieceTypeT, PieceT traits
│   ├── position/   # PositionT trait
│   └── rules/      # GameRules trait
├── standard/       # Standard 9×9 shogi type aliases
├── minishogi/      # Mini Shogi (5×5) variant
│   ├── position/   # Game state management
│   └── moves/      # Move types
├── sfen/           # SFEN parsing, formatting, and utilities
├── bitboard/       # Bitboard utilities and attack tables
├── position/       # Standard shogi Position implementation
└── ...             # Piece types, hand, moves, time control
```

## Key Types

| Type | Description |
|------|-------------|
| `Position` | Complete game state with move history |
| `Move` | Move representation (normal moves and drops) |
| `Square` | Board position (file, rank) |
| `Sfen` | Parsed SFEN string with components |
| `Bitboard` | Efficient set of squares for attack generation |
| `PieceType` | Piece types (Pawn, Lance, Knight, etc.) |
| `Piece` | Piece with color (PieceType + Color) |
| `Hand` | Captured pieces available for drops |
| `GameStatus` | Game state (InProgress, Checkmate, etc.) |
| `TimeControl` | Time management (Fischer, Byoyomi) |
| `NotationFormat` | Move notation style selection |

## SFEN Format

SFEN (Shogi Forsyth-Edwards Notation) is used for position serialization:

```
lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1
└─────────────────────┬─────────────────────┘ └┬┘└┬┘└┬┘
                 Board position            Side Hand Move#
```

- **Board**: Ranks separated by `/`, uppercase=Black, lowercase=White
- **Side to move**: `b` (Black/Sente) or `w` (White/Gote)
- **Hand pieces**: Captured pieces, `-` if none
- **Move number**: Full move counter

Piece characters: K(ing), R(ook), B(ishop), G(old), S(ilver), N(knight), L(ance), P(awn)
Promoted pieces: `+R` (Dragon), `+B` (Horse), `+S`, `+N`, `+L`, `+P` (Tokin)

The `Sfen` struct provides idiomatic parsing via `FromStr` and formatting via `Display`.
See the [SFEN Parsing example](#sfen-parsing-and-manipulation) above.

## Related Crates

- [csa-rs](https://github.com/nozaq/csa-rs): Shogi game serialization in CSA format
- [usi-rs](https://github.com/nozaq/usi-rs): Type-safe USI protocol communication

## Acknowledgements

Thanks to [nozaq](https://github.com/nozaq) for creating the original [shogi-rs](https://github.com/nozaq/shogi-rs) library, which provides the foundation for this fork.

## License

`shogi-rs` is licensed under the MIT license. See the [LICENSE](LICENSE) file for details.
