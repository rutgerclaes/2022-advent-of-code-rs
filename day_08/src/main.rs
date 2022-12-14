#![feature(iterator_try_collect, step_trait)]

use anyhow::Result;
use im::Vector;
use itertools::Itertools;
use model::Tree;
use tracing::{debug, trace};
use utils::{
    error::ProblemError,
    geom::{CompleteGrid, GridError, Point},
    input::read_lines,
    output::output,
};

fn main() {
    tracing_subscriber::fmt::init();

    let input: Vec<_> = read_lines("day_08/inputs/puzzle.txt").unwrap();
    let grid = parse_input(&input).unwrap();

    output("part 1", part_one(&grid));
    output("part 2", part_two(&grid));
}

fn parse_input(input: &[String]) -> Result<CompleteGrid<Tree>, GridError> {
    let trees: Result<Vector<_>, _> = input
        .iter()
        .map(|line| -> Result<Vector<_>, _> {
            line.chars()
                .map(|c| -> Result<Tree, _> { c.try_into() })
                .try_collect()
        })
        .try_collect();
    CompleteGrid::try_from(trees.unwrap())
}

fn part_one(forest: &CompleteGrid<Tree>) -> Result<usize> {
    let result = forest
        .iter()
        .filter(|(position, tree)| {
            let re = forest.row(position.y).unwrap();
            let ce = forest.col(position.x).unwrap();

            trace!("Evaluating tree {} at {}", tree, position);

            let visible_left = || re.iter().take(position.x).all(|o| o.height < tree.height);

            let visible_right = || {
                re.iter()
                    .skip(position.x + 1)
                    .all(|o| o.height < tree.height)
            };

            let visible_top = || ce.iter().take(position.y).all(|o| o.height < tree.height);

            let visible_bottom = || {
                ce.iter()
                    .skip(position.y + 1)
                    .all(|o| o.height < tree.height)
            };

            trace!("    Visible left: {}", visible_left());
            trace!("    Visible right: {}", visible_right());
            trace!("    Visible bottom: {}", visible_bottom());
            trace!("    Visible top: {}", visible_top());

            let r = visible_left() || visible_right() || visible_top() || visible_bottom();
            if r {
                debug!("  => Tree {} at {} is visible.", tree, position);
            } else {
                debug!("  => Tree {} at {} is *not* visible", tree, position);
            };
            r
        })
        .count();

    Ok(result)
}

fn scenic_score(position: &Point<usize>, tree: &Tree, forest: &CompleteGrid<Tree>) -> usize {
    if position.x == 0
        || position.x == forest.width()
        || position.y == 0
        || position.y == forest.height()
    {
        debug!("Scenic score of {} is 0 because of boundary", position);
        0
    } else {
        let re = forest.row(position.y).unwrap();
        let ce = forest.col(position.x).unwrap();

        let score_left = re
            .iter()
            .take(position.x)
            .rev()
            .find_position(|other| other.height >= tree.height)
            .map_or_else(|| position.x, |(score, _)| score + 1);

        let score_right = re
            .iter()
            .skip(position.x + 1)
            .find_position(|other| other.height >= tree.height)
            .map_or_else(|| forest.width() - position.x - 1, |(score, _)| score + 1);

        let score_top = ce
            .iter()
            .take(position.y)
            .rev()
            .find_position(|other| other.height >= tree.height)
            .map_or_else(|| position.y, |(score, _)| score + 1);

        let score_bottom = ce
            .iter()
            .skip(position.y + 1)
            .find_position(|other| other.height >= tree.height)
            .map_or_else(|| forest.height() - position.y - 1, |(score, _)| score + 1);

        let score = score_left * score_right * score_bottom * score_top;
        debug!("Scenic score of {} is {}", position, score);
        score
    }
}

fn part_two(forest: &CompleteGrid<Tree>) -> Result<usize> {
    let result = forest
        .iter()
        .map(|(position, tree)| scenic_score(&position, tree, forest))
        .max()
        .ok_or(ProblemError::NoSolutionFoundError)?;

    Ok(result)
}

#[cfg(test)]
mod tests {

    use tracing_test::traced_test;

    use super::*;

    #[test]
    #[traced_test]
    fn test_part_one_example() {
        let input: Vec<_> = ["30373", "25512", "65332", "33549", "35390"]
            .into_iter()
            .map(|s| s.to_owned())
            .collect();
        let forest = parse_input(&input).unwrap();
        let result = part_one(&forest).unwrap();
        assert_eq!(result, 21);
    }

    #[test]
    #[traced_test]
    fn test_scenic_score() {
        let input: Vec<_> = ["30373", "25512", "65332", "33549", "35390"]
            .into_iter()
            .map(|s| s.to_owned())
            .collect();
        let forest = parse_input(&input).unwrap();

        assert_eq!(scenic_score(&Point::new(2, 1), &Tree::new(5), &forest), 4);
        assert_eq!(scenic_score(&Point::new(2, 3), &Tree::new(5), &forest), 8);
    }

    #[test]
    #[traced_test]
    fn test_part_two_example() {
        let input: Vec<_> = ["30373", "25512", "65332", "33549", "35390"]
            .into_iter()
            .map(|s| s.to_owned())
            .collect();
        let forest = parse_input(&input).unwrap();
        let result = part_two(&forest).unwrap();
        assert_eq!(result, 8);
    }
}

mod model {
    use std::fmt::Display;
    use utils::error::ProblemError;

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct Tree {
        pub height: u8,
    }

    impl Display for Tree {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.height)
        }
    }

    impl TryFrom<char> for Tree {
        type Error = ProblemError;

        fn try_from(value: char) -> Result<Self, Self::Error> {
            value
                .to_digit(10)
                .map(|i| Tree::new(i as u8))
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!(
                        "Could not parse '{value}' into Tree height, only integers are supported"
                    ))
                })
        }
    }

    impl Tree {
        pub fn new(height: u8) -> Tree {
            Tree { height }
        }
    }
}
