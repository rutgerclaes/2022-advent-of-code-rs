use anyhow::Result;
use im::HashMap;
use itertools::Itertools;
use model::{Move, MoveError, World};
use tracing::info;
use utils::{input::read_lines, output::output};

use crate::model::{Crate, Stack};

fn main() {
    tracing_subscriber::fmt::init();

    let input: Vec<String> = read_lines("day_05/inputs/puzzle.txt").unwrap();

    output("part 1", part_one(&input));
    output("part 2", part_two(&input));
}

fn part_one(input: &[String]) -> Result<String> {
    let (mut world, moves) = parse_input_lines(input)?;
    world.apply_all(&moves)?;

    let top: Vec<_> = world.top_crates();
    Ok(list_crates(top))
}

fn part_two(input: &[String]) -> Result<String> {
    let (mut world, moves) = parse_input_lines(input)?;
    world.apply_all_multi(&moves)?;

    let top: Vec<_> = world.top_crates();
    Ok(list_crates(top))
}

fn list_crates(crates: Vec<Option<&Crate>>) -> String {
    crates
        .iter()
        .fold(String::from(""), |mut result, maybe| match maybe {
            Some(Crate(val)) => {
                result.push(*val);
                result
            }
            None => {
                info!("No top crate for one of the stacks");
                result
            }
        })
}

fn parse_input_lines(lines: &[String]) -> Result<(World, Vec<Move>)> {
    let mut i = lines.iter();
    let stack_lines = i
        .take_while_ref(|s| !s.is_empty())
        .filter(|s| s.contains('['))
        .collect_vec();
    let stacks = parse_stack_lines(&stack_lines);
    let moves: Result<Vec<Move>, MoveError> = i.skip(1).map(|s| s.parse::<Move>()).collect();

    Ok((World::new(stacks), moves?))
}

fn parse_stack_lines(lines: &[&String]) -> HashMap<usize, Stack> {
    lines.iter().fold(HashMap::new(), |stacks, line| {
        let crates = line
            .chars()
            .batching(|it| match it.next() {
                None => None,
                Some(' ') => {
                    it.dropping(3);
                    Some(None)
                }
                Some('[') => {
                    let ch = it.next().unwrap();
                    it.dropping(2);
                    Some(Some(Crate::new(ch)))
                }
                _ => panic!("Error parsing stack lines"),
            })
            .enumerate();

        crates.fold(stacks, |stacks, (index, maybe_crate)| {
            if let Some(crte) = maybe_crate {
                stacks.alter(
                    |stack| {
                        let mut stack = stack.unwrap_or_else(Stack::empty);
                        stack.build(crte);
                        Some(stack)
                    },
                    index,
                )
            } else {
                stacks
            }
        })
    })
}

mod model {
    use std::{fmt::Display, num::ParseIntError, str::FromStr};

    use anyhow::{Context, Result};
    use im::{vector, HashMap, Vector};
    use itertools::Itertools;
    use len_trait::{Empty, Len};
    use once_cell::unsync::Lazy;
    use regex::Regex;
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum CrateError {
        #[error("Error parsing crate: {0}")]
        ParseError(String),
    }

    #[derive(Error, Debug)]
    pub enum MoveError {
        #[error("Error parsing move: {0}")]
        ParseError(String),

        #[error("Error executing move: {0}")]
        InvalidMoveError(String),
    }

    impl From<ParseIntError> for MoveError {
        fn from(value: ParseIntError) -> Self {
            MoveError::ParseError(format!("Failed to parse int in move: {value}"))
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Crate(pub char);

    impl Crate {
        pub fn new(c: char) -> Crate {
            Crate(c)
        }
    }

    impl Display for Crate {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "[{}]", self.0)
        }
    }

    impl From<char> for Crate {
        fn from(value: char) -> Self {
            Crate(value)
        }
    }

    impl FromStr for Crate {
        type Err = CrateError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let regex = Lazy::new(|| Regex::new("^\\[(?P<id>[A-Z])\\]$").unwrap());
            match regex.captures(s) {
                Some(captures) => captures
                    .name("id")
                    .and_then(|c| c.as_str().chars().next())
                    .ok_or_else(|| CrateError::ParseError(format!("Id not found in '{s}'")))
                    .map(Crate),
                None => Err(CrateError::ParseError(format!("Regex did not match '{s}'"))),
            }
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub struct Move {
        amount: usize,
        source: usize,
        destination: usize,
    }

    impl Move {
        #[allow(dead_code)]
        pub fn new(amount: usize, source: usize, destination: usize) -> Move {
            Move {
                amount,
                source,
                destination,
            }
        }
    }

    impl Display for Move {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "move {} from {} to {}",
                self.amount, self.source, self.destination
            )
        }
    }

    impl FromStr for Move {
        type Err = MoveError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let regex = Lazy::new(|| {
                Regex::new(
                    "^move (?P<amount>[0-9]+) from (?P<source>[0-9]+) to (?P<destination>[0-9]+)$",
                )
                .unwrap()
            });
            match regex.captures(s) {
                Some(captures) => {
                    let amount: usize = captures
                        .name("amount")
                        .ok_or_else(|| {
                            MoveError::ParseError(format!(
                                "Could not get amount from {s}: {captures:?}",
                            ))
                        })?
                        .as_str()
                        .parse::<usize>()?;
                    let source: usize = captures
                        .name("source")
                        .ok_or_else(|| {
                            MoveError::ParseError(format!(
                                "Could not get source from {s}: {captures:?}"
                            ))
                        })?
                        .as_str()
                        .parse::<usize>()?;
                    let destination: usize = captures
                        .name("destination")
                        .ok_or_else(|| {
                            MoveError::ParseError(format!(
                                "Could not get destination from {s}: {captures:?}"
                            ))
                        })?
                        .as_str()
                        .parse::<usize>()?;
                    Ok(Move {
                        amount,
                        source,
                        destination,
                    })
                }
                None => Err(MoveError::ParseError(format!("Regex did not match '{s}'"))),
            }
        }
    }

    pub struct World {
        stacks: HashMap<usize, Stack>,
    }

    impl World {
        pub fn new(stacks: HashMap<usize, Stack>) -> World {
            World { stacks }
        }

        #[allow(dead_code)]
        pub fn stack(&mut self, index: usize) -> Option<&mut Stack> {
            self.stacks.get_mut(&(index - 1))
        }

        pub fn move_crate(&mut self, source: usize, destination: usize) -> Result<(), MoveError> {
            let source_stack = self.stacks.get_mut(&(source - 1)).ok_or_else(|| {
                MoveError::InvalidMoveError(format!("Cannot select stack {source} as source stack"))
            })?;

            if let Some(crte) = source_stack.pick() {
                let destination_stack =
                    self.stacks.get_mut(&(destination - 1)).ok_or_else(|| {
                        MoveError::InvalidMoveError(format!(
                            "Cannot select stack {destination} as destination stack"
                        ))
                    })?;
                destination_stack.drop(crte);
                Ok(())
            } else {
                Err(MoveError::InvalidMoveError(format!(
                    "Could not pick item from stack {source}"
                )))
            }
        }

        pub fn move_crates(&mut self, mve: &Move) -> Result<(), MoveError> {
            let source_stack = self.stacks.get_mut(&(mve.source - 1)).ok_or_else(|| {
                MoveError::InvalidMoveError(format!(
                    "Cannot select stack {} as source stack",
                    mve.source
                ))
            })?;
            let crates = source_stack.pick_crates(mve.amount)?;
            let destination_stack =
                self.stacks.get_mut(&(mve.destination - 1)).ok_or_else(|| {
                    MoveError::InvalidMoveError(format!(
                        "Cannot select stack {} as destination stack",
                        mve.destination
                    ))
                })?;
            destination_stack.drop_crates(crates);
            Ok(())
        }

        pub fn apply(&mut self, mve: &Move) -> Result<()> {
            for i in 0..(mve.amount) {
                self.move_crate(mve.source, mve.destination)
                    .context(format!("While performing move {i}: {mve}"))?;
            }

            Ok(())
        }

        pub fn apply_all(&mut self, moves: &[Move]) -> Result<()> {
            moves.iter().try_for_each(|mve| self.apply(mve))
        }

        pub fn apply_all_multi(&mut self, moves: &[Move]) -> Result<()> {
            moves
                .iter()
                .try_for_each(|mve| self.move_crates(mve).map_err(|err| err.into()))
        }

        pub fn top_crates<'a, I>(&'a self) -> I
        where
            I: FromIterator<Option<&'a Crate>>,
        {
            let max = *self.stacks.keys().max().unwrap_or(&0);
            (0..=max)
                .map(|i| self.stacks.get(&i).and_then(|stack| stack.top_crate()))
                .collect()
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Stack(Vector<Crate>);

    impl Stack {
        pub fn empty() -> Stack {
            Stack(vector![])
        }

        #[allow(dead_code)]
        pub fn from_str(s: &str) -> Stack {
            Stack(s.chars().map_into().collect())
        }

        pub fn drop(&mut self, crte: Crate) {
            self.0.push_back(crte)
        }

        pub fn drop_crates(&mut self, crates: Vector<Crate>) {
            self.0.append(crates)
        }

        pub fn build(&mut self, crte: Crate) {
            self.0.push_front(crte)
        }

        pub fn pick(&mut self) -> Option<Crate> {
            self.0.pop_back()
        }

        pub fn pick_crates(&mut self, amount: usize) -> Result<Vector<Crate>, MoveError> {
            if amount <= self.0.len() {
                Ok(self.0.split_off(self.0.len() - amount))
            } else {
                Err(MoveError::InvalidMoveError(format!(
                    "Insufficient crates to pick {amount} crates"
                )))
            }
        }

        pub fn top_crate(&self) -> Option<&Crate> {
            self.0.last()
        }
    }

    impl Empty for Stack {
        fn is_empty(&self) -> bool {
            self.0.is_empty()
        }
    }

    impl Len for Stack {
        fn len(&self) -> usize {
            self.0.len()
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_stack_pick() {
            let mut stack = Stack::from_str("ABC");
            assert_eq!(stack.pick(), Some(Crate::new('C')));
            assert_eq!(stack.pick(), Some(Crate::new('B')));
            assert_eq!(stack.pick(), Some(Crate::new('A')));
            assert_eq!(stack.pick(), None);
        }

        #[test]
        fn test_stack_drop() {
            let mut stack = Stack::empty();
            stack.drop(Crate::new('A'));
            stack.drop(Crate::new('B'));
            stack.drop(Crate::new('C'));
            stack.drop(Crate::new('D'));
            assert_eq!(stack, Stack::from_str("ABCD"));
        }

        #[test]
        fn test_stack_build() {
            let mut stack = Stack::empty();
            stack.build(Crate::new('A'));
            stack.build(Crate::new('B'));
            stack.build(Crate::new('C'));
            stack.build(Crate::new('D'));
            assert_eq!(stack, Stack::from_str("DCBA"));
        }

        #[test]
        fn test_stack_pick_multi() {
            let mut stack = Stack::from_str("ABCD");
            assert_eq!(
                stack.pick_crates(2).unwrap(),
                vector![Crate::new('C'), Crate::new('D')]
            );
            assert_eq!(stack.pick_crates(1).unwrap(), vector![Crate::new('B')]);
            assert_eq!(stack.pick_crates(1).unwrap(), vector![Crate::new('A')]);
            assert!(stack.pick_crates(1).is_err());
        }

        #[test]
        fn test_stack_drop_multi() {
            let mut stack = Stack::from_str("AB");
            stack.drop_crates(vector![Crate::new('C'), Crate::new('D')]);
            assert_eq!(stack, Stack::from_str("ABCD"));
        }
    }
}

#[cfg(test)]
mod tests {
    use utils::input::read_lines;

    use crate::{
        model::{Crate, Move, Stack},
        parse_input_lines,
    };

    #[test]
    fn test_world_parsing() {
        let input: Vec<_> = read_lines("inputs/example_1.txt").unwrap();
        let (mut world, moves) = parse_input_lines(&input).unwrap();

        assert_eq!(world.stack(1).unwrap(), &Stack::from_str("ZN"));
        assert_eq!(world.stack(2).unwrap(), &Stack::from_str("MCD"));
        assert_eq!(world.stack(3).unwrap(), &Stack::from_str("P"));

        assert_eq!(
            moves,
            vec![
                Move::new(1, 2, 1),
                Move::new(3, 1, 3),
                Move::new(2, 2, 1),
                Move::new(1, 1, 2),
            ]
        );
    }

    #[test]
    fn test_world_apply_moves() {
        let input: Vec<_> = read_lines("inputs/example_1.txt").unwrap();
        let (mut world, moves) = parse_input_lines(&input).unwrap();

        world.apply_all(&moves).unwrap();

        assert_eq!(world.stack(1).unwrap(), &Stack::from_str("C"));
        assert_eq!(world.stack(2).unwrap(), &Stack::from_str("M"));
        assert_eq!(world.stack(3).unwrap(), &Stack::from_str("PDNZ"));

        assert_eq!(
            world.top_crates::<Vec<_>>(),
            vec![
                Some(&Crate::new('C')),
                Some(&Crate::new('M')),
                Some(&Crate::new('Z'))
            ]
        )
    }
}
