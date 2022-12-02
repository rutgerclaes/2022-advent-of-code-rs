use anyhow::Result;
use model::Move;
use once_cell::unsync::Lazy;
use regex::Regex;
use utils::error::ProblemError;
use utils::input::read_lines;
use utils::output::output;

fn main() {
    tracing_subscriber::fmt::init();

    let input: Vec<String> = read_lines("day_02/inputs/puzzle.txt").unwrap();

    output("part 1", part_one(&input));
    output("part 2", part_two(&input));
}

fn part_one(lines: &[String]) -> Result<u32> {
    let mapper = |c: char| match c {
        'A' | 'X' => Some(Move::Rock),
        'B' | 'Y' => Some(Move::Paper),
        'C' | 'Z' => Some(Move::Scissors),
        _ => None,
    };

    let other_mapper = |c, _: &Move| mapper(c);

    calculate_score(lines, mapper, other_mapper)
}

fn part_two(lines: &[String]) -> Result<u32> {
    let mapper = |c: char| match c {
        'A' => Some(Move::Rock),
        'B' => Some(Move::Paper),
        'C' => Some(Move::Scissors),
        _ => None,
    };

    let other_mapper = |c: char, other: &Move| match c {
        'X' => Some(other.wins_against()),
        'Y' => Some(*other),
        'Z' => Some(other.loses_against()),
        _ => None,
    };

    calculate_score(lines, mapper, other_mapper)
}

fn calculate_score<F, G>(lines: &[String], mapper: F, other_mapper: G) -> Result<u32>
where
    F: Fn(char) -> Option<Move> + Copy,
    G: Fn(char, &Move) -> Option<Move> + Copy,
{
    let score = lines
        .iter()
        .map(|l| match parse_line(l, mapper, other_mapper) {
            Ok((other, own)) => Ok(own.score(&other)),
            Err(err) => Err(err),
        })
        .try_fold(0, |sum, score| score.map(|s| s as u32 + sum))?;
    Ok(score)
}

fn parse_line<F, G>(
    line: &str,
    first_mapper: F,
    second_mapper: G,
) -> Result<(Move, Move), ProblemError>
where
    F: Fn(char) -> Option<Move>,
    G: Fn(char, &Move) -> Option<Move>,
{
    let regex = Lazy::new(|| Regex::new("^(?P<other>[ABC]) (?P<self>[XYZ])$").unwrap());

    match regex.captures(line) {
        None => Err(ProblemError::InputParseError(format!(
            "Error parsing {}",
            line
        ))),
        Some(captures) => {
            let other = captures
                .name("other")
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!("No other found in {}", line))
                })?
                .as_str()
                .chars()
                .next()
                .unwrap();
            let other = first_mapper(other)
                .ok_or_else(|| ProblemError::InputParseError(format!("Unknown code: {}", other)))?;

            let own = captures
                .name("self")
                .ok_or_else(|| ProblemError::InputParseError(format!("No self found in {}", line)))?
                .as_str()
                .chars()
                .next()
                .unwrap();
            let own = second_mapper(own, &other)
                .ok_or_else(|| ProblemError::InputParseError(format!("Unknown code: {}", own)))?;

            Ok((other, own))
        }
    }
}

mod model {

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Move {
        Rock,
        Paper,
        Scissors,
    }

    impl Move {
        fn beats(&self, other: &Self) -> bool {
            other == &self.wins_against()
        }

        pub fn wins_against(&self) -> Self {
            match self {
                Move::Rock => Move::Scissors,
                Move::Paper => Move::Rock,
                Move::Scissors => Move::Paper,
            }
        }

        pub fn loses_against(&self) -> Self {
            match self {
                Move::Rock => Move::Paper,
                Move::Paper => Move::Scissors,
                Move::Scissors => Move::Rock,
            }
        }

        fn duel_score(&self, other: &Self) -> u8 {
            if self == other {
                3
            } else if self.beats(other) {
                6
            } else {
                0
            }
        }

        fn move_score(&self) -> u8 {
            match self {
                Move::Rock => 1,
                Move::Paper => 2,
                Move::Scissors => 3,
            }
        }

        pub fn score(&self, other: &Self) -> u8 {
            self.duel_score(other) + self.move_score()
        }
    }

    #[cfg(test)]
    mod test {

        use super::*;

        #[test]
        fn test_score_calculation() {
            assert_eq!( Move::Paper.score( &Move::Rock ), 8 );
            assert_eq!( Move::Rock.score( &Move::Paper ), 1 );
            assert_eq!( Move::Scissors.score( &Move::Scissors ), 6 );
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_part_one() {
        let lines = [ String::from("A Y"), String::from("B X"), String::from("C Z") ];
        assert_eq!( part_one(&lines).unwrap(), 15 );
    }
    
    #[test]
    fn test_part_two() {
        let lines = [ String::from("A Y"), String::from("B X"), String::from("C Z") ];
        assert_eq!( part_two(&lines).unwrap(), 12 );
    }
}