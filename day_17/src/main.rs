#![feature(iterator_try_collect)]

use model::{Cave, Instruction, RockType};
use tailcall::tailcall;
use tracing::{debug, trace};
use utils::{input::read_file, output};

use crate::model::PatternHistory;

fn main() {
    tracing_subscriber::fmt::init();

    let instructions: Vec<Instruction> = read_file("day_17/inputs/puzzle.txt")
        .unwrap()
        .chars()
        .map(|c| c.try_into())
        .try_collect()
        .unwrap();

    output::output_success("part 1", part_one(&instructions));
    output::output_success("part 2", part_two(&instructions));
}

fn part_one(instructions: &[Instruction]) -> usize {
    let infinite_instructions = instructions.iter().enumerate().cycle();
    let falling_rocks = RockType::sequence().into_iter().cycle();

    let cave = Cave::empty();

    let (cave, _) = falling_rocks.take(2022).enumerate().fold(
        (cave, infinite_instructions),
        |(mut cave, mut instructions), (_idx, rock)| {
            cave.drop(rock, &mut instructions, false);

            (cave, instructions)
        },
    );

    cave.height()
}

fn part_two(instructions: &[Instruction]) -> usize {
    let rocks = RockType::sequence();
    let infinite_instructions = instructions.iter().enumerate().cycle();
    let falling_rocks = rocks.into_iter().cycle();

    let cave = Cave::empty();

    #[tailcall]
    fn simulate<'a>(
        iteration: usize,
        blocks_to_take: usize,
        mut cave: Cave,
        mut instructions: impl Iterator<Item = (usize, &'a Instruction)>,
        mut rocks: impl Iterator<Item = RockType>,
        mut history: PatternHistory,
    ) -> Cave {
        if blocks_to_take > 0 {
            let rock = rocks.next().unwrap();
            let (initial_instruction, maybe_pattern) = cave.drop(rock, &mut instructions, false);

            if let Some(pattern) = maybe_pattern {
                let key = (pattern, initial_instruction, rock);
                if let Some((prev_iteration, prev_max_y)) = history.get(&key) {
                    let cycle_length = iteration - prev_iteration;
                    if cycle_length < blocks_to_take {
                        let height_increase = cave.max_y - prev_max_y;
                        debug!( "Pattern detected in iteration {} cycle length is {}, height increase is {} (now {}, was {}).", iteration, cycle_length, height_increase, cave.max_y, prev_max_y );
                        let nb_of_cycles = blocks_to_take / cycle_length;
                        debug!(
                            "Fast forwarding {} cycles ({} iterations)",
                            nb_of_cycles,
                            nb_of_cycles * cycle_length
                        );
                        let total_height_increase = height_increase * nb_of_cycles;
                        let old_height = cave.height();
                        cave.apply_pattern(total_height_increase, &key.0);
                        debug!(
                            "Total height increase is {}: new height is {} (from {})",
                            total_height_increase,
                            cave.height(),
                            old_height
                        );
                        debug!(
                            "Continuing with {} ({} blocks left)",
                            iteration + 1 + nb_of_cycles * cycle_length,
                            blocks_to_take - 1 - nb_of_cycles * cycle_length
                        );
                        simulate(
                            iteration + 1 + nb_of_cycles * cycle_length,
                            blocks_to_take - 1 - nb_of_cycles * cycle_length,
                            cave,
                            instructions,
                            rocks,
                            history,
                        )
                    } else {
                        debug!( "Pattern detected in iteration {} cycle length is {}, cycle length is too large", iteration, iteration - prev_iteration );
                        simulate(
                            iteration + 1,
                            blocks_to_take - 1,
                            cave,
                            instructions,
                            rocks,
                            history,
                        )
                    }
                } else {
                    history.record(key, (iteration, cave.max_y));
                    simulate(
                        iteration + 1,
                        blocks_to_take - 1,
                        cave,
                        instructions,
                        rocks,
                        history,
                    )
                }
            } else {
                trace!( "No recognizable pattern obtained from rock drop, continuing with {} ({} rocks left)", iteration + 1, blocks_to_take - 1 );
                simulate(
                    iteration + 1,
                    blocks_to_take - 1,
                    cave,
                    instructions,
                    rocks,
                    history,
                )
            }
        } else {
            cave
        }
    }

    let end = simulate(
        0,
        1000000000000,
        cave,
        infinite_instructions,
        falling_rocks,
        PatternHistory::empty(),
    );
    end.height()
}

mod model {
    use std::{
        collections::{HashMap, HashSet},
        fmt::Display,
    };

    use tracing::debug;
    use utils::{
        error::ProblemError,
        geom::{Direction, Point},
    };

    #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
    pub enum RockType {
        Minus,
        Plus,
        Angle,
        Pipe,
        Square,
    }

    impl RockType {
        pub fn sequence() -> [RockType; 5] {
            [
                Self::Minus,
                Self::Plus,
                Self::Angle,
                Self::Pipe,
                Self::Square,
            ]
        }

        fn positions(&self) -> Vec<Point<i64>> {
            match self {
                Self::Minus => vec![
                    Point::new(0, 0),
                    Point::new(1, 0),
                    Point::new(2, 0),
                    Point::new(3, 0),
                ],
                Self::Plus => vec![
                    Point::new(1, 0),
                    Point::new(0, 1),
                    Point::new(1, 1),
                    Point::new(2, 1),
                    Point::new(1, 2),
                ],
                Self::Angle => vec![
                    Point::new(0, 0),
                    Point::new(1, 0),
                    Point::new(2, 0),
                    Point::new(2, 1),
                    Point::new(2, 2),
                ],
                Self::Pipe => vec![
                    Point::new(0, 0),
                    Point::new(0, 1),
                    Point::new(0, 2),
                    Point::new(0, 3),
                ],
                Self::Square => vec![
                    Point::new(0, 0),
                    Point::new(1, 0),
                    Point::new(0, 1),
                    Point::new(1, 1),
                ],
            }
        }

        fn offset<I>(&self, offset: &Point<i64>) -> I
        where
            I: FromIterator<Point<i64>>,
        {
            self.positions().iter().map(move |&p| p + offset).collect()
        }
    }

    pub struct PatternHistory(HashMap<(Pattern, usize, RockType), (usize, usize)>);

    impl PatternHistory {
        pub fn empty() -> Self {
            PatternHistory(HashMap::new())
        }

        pub fn record(&mut self, key: (Pattern, usize, RockType), value: (usize, usize)) {
            self.0.insert(key, value);
        }

        pub fn get(&self, key: &(Pattern, usize, RockType)) -> Option<&(usize, usize)> {
            self.0.get(key)
        }
    }

    #[derive(Debug, Hash, Clone, Eq, PartialEq)]
    pub struct Pattern([[bool; 7]; 20]);

    impl Display for Pattern {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for row in self.0 {
                write!(f, "|")?;
                for e in row {
                    write!(f, "{}", if e { '#' } else { '.' })?;
                }
                writeln!(f, "|")?;
            }

            Ok(())
        }
    }

    pub struct Cave {
        pub positions: HashSet<Point<i64>>,
        pub max_y: usize,
    }

    impl Cave {
        pub fn empty() -> Cave {
            Cave {
                positions: HashSet::new(),
                max_y: 0,
            }
        }

        pub fn height(&self) -> usize {
            self.max_y + 1
        }

        fn blocks(&self, position: &Point<i64>) -> bool {
            position.x < 0 || position.x >= 7 || position.y < 0 || self.positions.contains(position)
        }

        fn next_rock_insertion(&self) -> Point<i64> {
            Point::new(
                2,
                self.max_y as i64 + 3 + if self.positions.is_empty() { 0 } else { 1 },
            )
        }

        fn pattern(&self) -> Option<Pattern> {
            if self.max_y >= 20 {
                let mut pattern = [[false; 7]; 20];

                (0..pattern.len()).for_each(|y| {
                    (0..7).for_each(|x| {
                        let point: Point<i64> = Point::new(x as i64, self.max_y as i64 - y as i64);
                        pattern[y][x] = self.positions.contains(&point)
                    });
                });

                Some(Pattern(pattern))
            } else {
                None
            }
        }

        pub fn apply_pattern(&mut self, offset: usize, pattern: &Pattern) {
            let old = self.max_y;
            for (y, row) in pattern.0.iter().enumerate() {
                for (x, &e) in row.iter().enumerate() {
                    if e {
                        let p = Point::new(x as i64, (old + offset - y) as i64);
                        self.insert(p);
                        debug!("Inserted {}, max height now {}", p, self.max_y);
                    }
                }
            }
        }

        pub fn insert(&mut self, point: Point<i64>) {
            self.positions.insert(point);
            if self.max_y < point.y as usize {
                self.max_y = point.y as usize;
            }
        }

        pub fn drop<'a>(
            &mut self,
            rock_type: RockType,
            jets: &mut impl Iterator<Item = (usize, &'a Instruction)>,
            debug: bool,
        ) -> (usize, Option<Pattern>) {
            let position = self.next_rock_insertion();
            if debug {
                println!("Rock begins falling:");
                self.animate(&rock_type.offset(&position), '@');
            }

            let (final_instruction, final_position) =
                self.drop_from(position, &rock_type, jets, debug);

            let final_positions: Vec<_> = rock_type.offset(&final_position);
            final_positions.into_iter().for_each(|p| {
                self.insert(p);
            });

            (final_instruction, self.pattern())
        }

        fn animate(&self, moving_positions: &HashSet<Point<i64>>, moving_char: char) {
            let moving_top = moving_positions
                .iter()
                .map(|p| p.y as usize)
                .max()
                .unwrap_or(0);
            let absolute_top = if moving_top > self.max_y {
                moving_top
            } else {
                self.max_y
            };

            (0..=absolute_top).rev().for_each(|y| {
                print!("|");
                (0..7).for_each(|x| {
                    let point = Point::new(x as i64, y as i64);
                    let char = if moving_positions.contains(&point) {
                        moving_char
                    } else if self.positions.contains(&point) {
                        '#'
                    } else {
                        '.'
                    };

                    print!("{char}");
                });
                println!("|");
            });
            println!("+-------+\n");
        }

        fn drop_from<'a>(
            &mut self,
            position: Point<i64>,
            rock_type: &RockType,
            jets: &mut impl Iterator<Item = (usize, &'a Instruction)>,
            debug: bool,
        ) -> (usize, Point<i64>) {
            let (instruction_idx, instruction) = jets.next().unwrap();
            let maybe_position_after_jet = position.direction(&instruction.into(), 1);

            let maybe_positions_after_jet: Vec<_> = rock_type.offset(&maybe_position_after_jet);

            let position_after_jet = if maybe_positions_after_jet.iter().any(|pos| self.blocks(pos))
            {
                if debug {
                    println!(
                        "Jet of gas pushes rock {instruction:?} but nothing happens: ({position})"
                    );
                    self.animate(&rock_type.offset(&position), '@');
                }
                position
            } else {
                if debug {
                    println!(
                        "Jet of gas pushes rock {instruction:?}: ({maybe_position_after_jet})"
                    );
                    self.animate(&maybe_positions_after_jet.iter().copied().collect(), '@');
                }
                maybe_position_after_jet
            };

            let positions_after_jet: Vec<_> = rock_type.offset(&position_after_jet);
            let maybe_position_after_drop = position_after_jet.direction(&Direction::Down, 1);
            let maybe_positions_after_drop: Vec<_> = rock_type.offset(&maybe_position_after_drop);

            if maybe_positions_after_drop.iter().any(|p| self.blocks(p)) {
                if debug {
                    println!("Rock comes to rest: ({position_after_jet})");
                    self.animate(&positions_after_jet.iter().copied().collect(), '#');
                }

                (instruction_idx, position_after_jet)
            } else {
                if debug {
                    println!("Rock falls 1 unit: ({maybe_position_after_drop})");
                    self.animate(&maybe_positions_after_drop.iter().copied().collect(), '@');
                }
                self.drop_from(maybe_position_after_drop, rock_type, jets, debug)
            }
        }
    }

    impl Display for Cave {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            (0..self.max_y).rev().try_for_each(|y| {
                write!(f, "{y:04} |")?;
                (0..7).try_for_each(|x| {
                    let char = if self.positions.contains(&Point::new(x, y as i64)) {
                        '#'
                    } else {
                        '.'
                    };
                    write!(f, "{char}")
                })?;
                writeln!(f, "|")
            })?;
            writeln!(f, "     +0123456+")
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum Instruction {
        Left,
        Right,
    }

    impl TryFrom<char> for Instruction {
        type Error = ProblemError;

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                '>' => Ok(Self::Right),
                '<' => Ok(Self::Left),
                c => Err(ProblemError::InputParseError(format!(
                    "Error converting {c} to Instruction"
                ))),
            }
        }
    }

    impl From<&Instruction> for Direction {
        fn from(value: &Instruction) -> Self {
            match value {
                Instruction::Left => Self::Left,
                Instruction::Right => Self::Right,
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_part_one() {
        let instructions: Vec<Instruction> = read_file("inputs/example.txt")
            .unwrap()
            .chars()
            .map(|c| c.try_into())
            .try_collect()
            .unwrap();

        let result = part_one(&instructions);
        assert_eq!(result, 3068);
    }

    #[test]
    fn test_part_two() {
        let instructions: Vec<Instruction> = read_file("inputs/example.txt")
            .unwrap()
            .chars()
            .map(|c| c.try_into())
            .try_collect()
            .unwrap();

        let result = part_two(&instructions);
        assert_eq!(result, 1514285714288);
    }
}
