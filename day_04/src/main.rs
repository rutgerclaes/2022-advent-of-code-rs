use anyhow::Result;
use model::Pair;
use utils::input::parse_lines;
use utils::output::output;

fn main() {
    tracing_subscriber::fmt::init();

    let input: Vec<Pair> = parse_lines("day_04/inputs/puzzle.txt").unwrap();

    output("part 1", part_one(&input));
    output("part 2", part_two(&input));
}

fn part_one( pairs: &[Pair] ) -> Result<usize> {
    Ok( pairs.iter().filter( |p| p.overlaps_fully() ).count() )
}

fn part_two( pairs: &[Pair] ) -> Result<usize> {
    Ok( pairs.iter().filter( |p| p.overlaps() ).count() )
}

mod model {
    use std::{ops::RangeInclusive, str::FromStr, num::ParseIntError};

    use once_cell::unsync::Lazy;
    use regex::Regex;
    use thiserror::Error;

    #[derive(Debug)]
    pub struct Pair(RangeInclusive<u32>, RangeInclusive<u32>);

    impl Pair {
        pub fn overlaps_fully(&self) -> bool {
            fn overlaps(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
                b.contains(a.start()) && b.contains(a.end())
            }

            overlaps(&self.0, &self.1) || overlaps(&self.1, &self.0)
        }
        
        pub fn overlaps(&self) -> bool {
            fn overlaps(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
                b.contains(a.start()) || b.contains(a.end())
            }

            overlaps(&self.0, &self.1) || overlaps(&self.1, &self.0)
        }
    }

    #[derive(Error,Debug)]
    pub enum PairError {
        #[error("Failed to parse pair: {0}")]
        ParseError( String )
    }

    impl From<ParseIntError> for PairError {
        fn from(value: ParseIntError) -> Self {
            PairError::ParseError( format!( "Failed to parse boundary: {}", value ) )
        }
    }

    impl FromStr for Pair {
        type Err = PairError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let regex = Lazy::new(|| {
                Regex::new("^(?P<first_start>[0-9]+)-(?P<first_end>[0-9]+),(?P<second_start>[0-9]+)-(?P<second_end>[0-9]+)$").unwrap()
            });
            match regex.captures(s) {
                Some(captures) => {
                    let first_start = captures
                        .name("first_start")
                        .ok_or_else(|| {
                            PairError::ParseError(format!(
                                "first_start not found in '{}'",
                                s
                            ))
                        })?
                        .as_str()
                        .parse::<u32>()?;
                    let first_end = captures
                        .name("first_end")
                        .ok_or_else(|| {
                            PairError::ParseError(format!("first_end not found in '{}'", s))
                        })?
                        .as_str()
                        .parse::<u32>()?;
                    let second_start = captures
                        .name("second_start")
                        .ok_or_else(|| {
                            PairError::ParseError(format!("first_end not found in '{}'", s))
                        })?
                        .as_str()
                        .parse::<u32>()?;
                    let second_end = captures
                        .name("second_end")
                        .ok_or_else(|| {
                            PairError::ParseError(format!(
                                "second_end not found in '{}'",
                                s
                            ))
                        })?
                        .as_str()
                        .parse::<u32>()?;

                    Ok(Pair(first_start..=first_end, second_start..=second_end))
                }
                None => Err(PairError::ParseError(format!(
                    "Failed to parse input line {}",
                    s
                ))),
            }
        }
    }

    #[cfg(test)]
    mod tests {

        use super::*;

        #[test]
        fn test_overlap_fully() {
            let pair = Pair(2..=4, 6..=8);
            assert_eq!(pair.overlaps_fully(), false);

            let pair = Pair(2..=3, 4..=5);
            assert_eq!(pair.overlaps_fully(), false);

            let pair = Pair(5..=7, 7..=9);
            assert_eq!(pair.overlaps_fully(), false);

            let pair = Pair(2..=8, 3..=7);
            assert_eq!(pair.overlaps_fully(), true);

            let pair = Pair(6..=6, 4..=6);
            assert_eq!(pair.overlaps_fully(), true);

            let pair = Pair(2..=6, 4..=8);
            assert_eq!(pair.overlaps_fully(), false);
        }
        
        #[test]
        fn test_overlap() {
            let pair = Pair(2..=4, 6..=8);
            assert_eq!(pair.overlaps_fully(), false);

            let pair = Pair(2..=3, 4..=5);
            assert_eq!(pair.overlaps_fully(), false);

            let pair = Pair(5..=7, 7..=9);
            assert_eq!(pair.overlaps_fully(), true);

            let pair = Pair(2..=8, 3..=7);
            assert_eq!(pair.overlaps_fully(), true);

            let pair = Pair(6..=6, 4..=6);
            assert_eq!(pair.overlaps_fully(), true);

            let pair = Pair(2..=6, 4..=8);
            assert_eq!(pair.overlaps_fully(), true);
        }
    }
}
