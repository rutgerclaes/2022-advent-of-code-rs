#![feature(is_some_and)]

use anyhow::Result;
use itertools::Itertools;
use model::Calories;
use tracing::{event, Level};
use utils::error::ProblemError;
use utils::input::read_lines;
use utils::output::output;

fn main() {
    tracing_subscriber::fmt::init();
    let calories: Vec<String> = read_lines("day_01/inputs/puzzle.txt").unwrap();

    match parse_lines(&calories) {
        Err(error) => event!(Level::ERROR, "Error grouping calories: {}", error),
        Ok(grouped) => {
            output("part 1", part_one(&grouped));
            output("part 2", part_two(&grouped));
        }
    }
}

fn parse_lines(lines: &[String]) -> Result<Vec<Vec<Calories>>> {
    Ok(lines
        .iter()
        .batching(|it| {
            let next_group = it.take_while(|s| !s.is_empty());
            let res: Result<Vec<Calories>, _> = next_group.map(|val| val.parse()).try_collect();
            if res.as_ref().is_ok_and(|l| !l.is_empty()) {
                Some(res)
            } else {
                None
            }
        })
        .try_collect()?)
}

fn part_one(calories: &[Vec<Calories>]) -> Result<Calories> {
    calories
        .iter()
        .map(|list| list.iter().sum())
        .max()
        .ok_or(ProblemError::NoSolutionFoundError.into())
}

fn part_two(calories: &[Vec<Calories>]) -> Result<Calories> {
    let mut sums: Vec<Calories> = calories.iter().map(|list| list.iter().sum()).collect();
    sums.sort_by_key(|k| -(k.0 as i64));
    Ok(sums.iter().take(3).sum())
}

mod model {
    use std::{fmt::Display, iter::Sum, ops::Add, str::FromStr};

    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum CaloriesError {
        #[error("Failed to parse calories value '{0}': {1}")]
        ParseError(String, String),
    }

    #[derive(PartialEq, Debug, Eq, Copy, Clone)]
    pub struct Calories(pub u64);

    impl Display for Calories {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "calories: {}", self.0)
        }
    }

    impl FromStr for Calories {
        type Err = CaloriesError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let value = s
                .parse::<u64>()
                .map_err(|err| CaloriesError::ParseError(s.to_owned(), err.to_string()))?;

            Ok(Calories(value))
        }
    }

    impl Add<Calories> for Calories {
        type Output = Calories;

        fn add(self, rhs: Calories) -> Self::Output {
            Calories(self.0 + rhs.0)
        }
    }

    impl<'a> Sum<&'a Calories> for Calories {
        fn sum<I: Iterator<Item = &'a Calories>>(iter: I) -> Self {
            let result: Option<Calories> = iter.copied().reduce(|a, b| a + b);
            result.unwrap_or(Calories(0))
        }
    }

    impl PartialOrd for Calories {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            self.0.partial_cmp(&other.0)
        }
    }

    impl Ord for Calories {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.cmp(&other.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::model::*;
    use super::*;

    #[test]
    fn test_calorie_grouping() {
        let input = [
            "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "",
            "10000",
        ]
        .map(|s| s.to_owned());
        let groups = parse_lines(&input).unwrap();

        assert_eq!(
            groups,
            vec![
                vec![Calories(1000), Calories(2000), Calories(3000)],
                vec![Calories(4000)],
                vec![Calories(5000), Calories(6000)],
                vec![Calories(7000), Calories(8000), Calories(9000)],
                vec![Calories(10000)]
            ]
        )
    }

    #[test]
    fn test_part_one() {
        let input = [
            "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "",
            "10000",
        ]
        .map(|s| s.to_owned());

        let groups = parse_lines(&input).unwrap();

        let result = part_one(&groups).unwrap();
        assert_eq!(result, Calories(24000));
    }

    #[test]
    fn test_part_two() {
        let input = [
            "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "",
            "10000",
        ]
        .map(|s| s.to_owned());

        let groups = parse_lines(&input).unwrap();

        let result = part_two(&groups).unwrap();
        assert_eq!(result, Calories(24000 + 11000 + 10000));
    }
}
