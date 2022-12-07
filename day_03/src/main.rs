use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools;
use model::Rucksack;
use utils::error::ProblemError;
use utils::input::read_lines;
use utils::output::output;

use crate::model::Item;

fn main() {
    tracing_subscriber::fmt::init();

    let input: Vec<String> = read_lines("day_03/inputs/puzzle.txt").unwrap();
    let sacks: Vec<Rucksack> = input.iter().map_into().collect();

    output("part 1", part_one(&sacks));
    output("part 2", part_two(&sacks));
}

fn part_one(rucksacks: &[Rucksack]) -> Result<u32> {
    Ok(rucksacks
        .iter()
        .map(|r| r.common_item(2).ok_or(ProblemError::NoSolutionFoundError))
        .fold_ok(0u32, |sum, item| sum + item.priority())?)
}

fn part_two(rucksacks: &[Rucksack]) -> Result<u32> {
    Ok(rucksacks
        .iter()
        .tuples()
        .map(|(a, b, c)| {
            let common: HashSet<&Item> = a.items.iter().collect();
            let common: HashSet<&Item> = common
                .intersection(&b.items.iter().collect())
                .copied()
                .collect();
            let common: HashSet<&Item> = common
                .intersection(&c.items.iter().collect())
                .copied()
                .collect();
            common
                .iter()
                .exactly_one()
                .map(|&item| item.priority())
                .map_err(|_| ProblemError::NoSolutionFoundError)
        })
        .fold_ok(0, |sum, prio| sum + prio)?)
}

mod model {
    use std::{collections::HashSet, fmt::Display};

    use itertools::Itertools;
    use len_trait::Len;
    use tracing::warn;

    #[derive(Debug, PartialEq, Hash, Clone, Copy, Eq)]
    pub struct Item(char);

    impl Item {
        pub fn priority(&self) -> u32 {
            let val: u32 = self.0.into();
            match val {
                65..=90 => val - 65 + 27,
                97..=122 => val - 96,
                _ => panic!("Invalid item char: {}", self.0),
            }
        }
    }

    impl From<char> for Item {
        fn from(value: char) -> Self {
            Item(value)
        }
    }

    impl Display for Item {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Debug)]
    pub struct Rucksack {
        pub items: Vec<Item>,
    }

    impl Rucksack {
        fn new<I>(items: I) -> Rucksack
        where
            I: Iterator<Item = Item>,
        {
            Rucksack {
                items: items.into_iter().collect(),
            }
        }

        fn partition<'a, I, J>(&'a self, n: usize) -> I
        where
            I: FromIterator<J>,
            J: FromIterator<&'a Item> + Len,
        {
            if self.items.len() % n != 0 {
                warn!(
                    "Partitioning items ({}) in rucksack unevenly into {} partitions",
                    self.items.len(),
                    n
                )
            }

            let partition_size = self.items.len() / n;

            let partitions: I = self
                .items
                .iter()
                .batching(|i| {
                    let vec: J = i.take(partition_size).collect();
                    if vec.len() == 0 {
                        None
                    } else {
                        Some(vec)
                    }
                })
                .collect();

            partitions
        }

        pub fn common_item(&self, n: usize) -> Option<&Item> {
            let partitions: Vec<HashSet<&Item>> = self.partition(n);
            let common = partitions
                .into_iter()
                .reduce(|a, b| a.intersection(&b).copied().collect());
            match common {
                None => None,
                Some(result) => {
                    if let Ok(out) = result.iter().exactly_one() {
                        Some(out)
                    } else {
                        None
                    }
                }
            }
        }
    }

    impl From<&String> for Rucksack {
        fn from(value: &String) -> Self {
            Rucksack::new(value.chars().map_into())
        }
    }

    impl From<&str> for Rucksack {
        fn from(value: &str) -> Self {
            Rucksack::new(value.chars().map_into())
        }
    }
    #[cfg(test)]
    mod tests {

        use super::*;

        #[test]
        fn test_priority() {
            assert_eq!(Item('a').priority(), 1);
            assert_eq!(Item('z').priority(), 26);
            assert_eq!(Item('A').priority(), 27);
            assert_eq!(Item('Z').priority(), 52);
        }

        #[test]
        fn test_partition() {
            let rucksack = Rucksack::new("aAbzZA".chars().map(|c| Item(c)));

            let partitions: Vec<Vec<_>> = rucksack.partition(1);
            assert_eq!(
                partitions,
                vec![vec![
                    &Item('a'),
                    &Item('A'),
                    &Item('b'),
                    &Item('z'),
                    &Item('Z'),
                    &Item('A')
                ]]
            );

            let partitions: Vec<Vec<_>> = rucksack.partition(2);
            assert_eq!(
                partitions,
                vec![
                    vec![&Item('a'), &Item('A'), &Item('b')],
                    vec![&Item('z'), &Item('Z'), &Item('A')]
                ]
            );

            let partitions: Vec<Vec<_>> = rucksack.partition(3);
            assert_eq!(
                partitions,
                vec![
                    vec![&Item('a'), &Item('A')],
                    vec![&Item('b'), &Item('z')],
                    vec![&Item('Z'), &Item('A')]
                ]
            );
        }

        #[test]
        fn test_common_item() {
            let rucksack = Rucksack::new("aAbzZA".chars().map(|c| Item(c)));

            let common = rucksack.common_item(1);
            assert_eq!(common, None);

            let common = rucksack.common_item(2);
            assert_eq!(common, Some(&Item('A')));

            let common = rucksack.common_item(3);
            assert_eq!(common, None);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;


    #[test]
    fn test_part_one() {
        let input = [
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw",
        ];

        let rucksacks: Vec<Rucksack> = input.into_iter().map_into().collect();
        let result = part_one( &rucksacks ).unwrap();

        assert_eq!( result, 157 )
    }

    #[test]
    fn test_part_two() {
        let input = [
            "vJrwpWtwJgWrhcsFMMfFFhFp",
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
            "PmmdzqPrVvPwwTWBwg",
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
            "ttgJtRGJQctTZtZT",
            "CrZsJsPPZsGzwwsLwLmpwMDw",
        ];

        let rucksacks: Vec<Rucksack> = input.into_iter().map_into().collect();
        let result = part_two( &rucksacks ).unwrap();

        assert_eq!( result, 70 )
    }
}
