use anyhow::Result;
use im::{hashset, HashSet, Vector};
use model::Square;
use tracing::debug;
use utils::{
    error::ProblemError,
    geom::{CompleteGrid, GridError, Point},
    input::read_lines,
    output::output,
};

fn main() {
    tracing_subscriber::fmt::init();
    let grid: CompleteGrid<Square> = read_lines("day_12/inputs/puzzle.txt")
        .and_then(|lines: Vector<_>| lines.try_into().map_err(|err: GridError| err.into()))
        .unwrap();
    output("part 1", part_one(&grid));
    output("part 2", part_two(&grid));
}

fn part_one(grid: &CompleteGrid<Square>) -> Result<usize> {
    fn flood(
        grid: &CompleteGrid<Square>,
        front: HashSet<Point<usize>>,
        seen: HashSet<Point<usize>>,
        current_distance: usize,
    ) -> Option<usize> {
        let new_front: HashSet<Point<usize>> = front
            .iter()
            .flat_map(|point| {
                let current_square = grid.get(point).unwrap();
                let neighbours: Vec<(Point<usize>, &Square)> = grid.neighbours(point);
                neighbours
                    .into_iter()
                    .filter(|(new_point, new_square)| {
                        !seen.contains(new_point)
                            && new_square.height() <= current_square.height() + 1
                    })
                    .map(|(point, _)| point)
            })
            .collect();

        let destination = new_front
            .iter()
            .find(|e| grid.get(e).map(|s| s.is_end()).unwrap_or(false));
        if let Some(destination_location) = destination {
            debug!(
                "Found destination at {}, in iteration {}",
                destination_location, current_distance
            );
            Some(current_distance + 1)
        } else if !new_front.is_empty() {
            flood(
                grid,
                new_front.clone(),
                seen + new_front,
                current_distance + 1,
            )
        } else {
            None
        }
    }

    let (start, _) = grid
        .iter()
        .find(|(_, square)| square.is_start())
        .ok_or(ProblemError::InputAssumptionFailedError(
            "No start location found in grid",
        ))
        .unwrap();
    debug!("Start location found at {}", start);

    Ok(flood(grid, hashset![start], hashset![start], 0)
        .ok_or(ProblemError::NoSolutionFoundError)?)
}

fn part_two(grid: &CompleteGrid<Square>) -> Result<usize> {
    fn flood(
        grid: &CompleteGrid<Square>,
        front: HashSet<Point<usize>>,
        seen: HashSet<Point<usize>>,
        current_distance: usize,
    ) -> Option<usize> {
        let new_front: HashSet<Point<usize>> = front
            .iter()
            .flat_map(|point| {
                let current_square = grid.get(point).unwrap();
                let neighbours: Vec<(Point<usize>, &Square)> = grid.neighbours(point);
                neighbours
                    .into_iter()
                    .filter(|(new_point, new_square)| {
                        !seen.contains(new_point)
                            && current_square.height() <= new_square.height() + 1
                    })
                    .map(|(point, _)| point)
            })
            .collect();

        let destination = new_front
            .iter()
            .find(|e| grid.get(e).map(|s| s.height() == 0).unwrap_or(false));
        if let Some(destination_location) = destination {
            debug!(
                "Found destination at {}, in iteration {}",
                destination_location, current_distance
            );
            Some(current_distance + 1)
        } else if !new_front.is_empty() {
            flood(
                grid,
                new_front.clone(),
                seen + new_front,
                current_distance + 1,
            )
        } else {
            None
        }
    }

    let (dest, _) = grid
        .iter()
        .find(|(_, square)| square.is_end())
        .ok_or(ProblemError::InputAssumptionFailedError(
            "No start location found in grid",
        ))
        .unwrap();
    debug!("Destination location found at {}", dest);

    Ok(flood(grid, hashset![dest], hashset![dest], 0).ok_or(ProblemError::NoSolutionFoundError)?)
}

mod model {
    use std::fmt::Display;

    use utils::error::ProblemError;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Square {
        Start,
        Destination,
        NormalSquare { height: u8 },
    }

    impl Display for Square {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.char())
        }
    }

    impl From<Square> for char {
        fn from(value: Square) -> Self {
            value.char()
        }
    }

    impl Square {
        pub fn is_start(&self) -> bool {
            matches!(self, Self::Start)
        }

        pub fn is_end(&self) -> bool {
            matches!(self, Self::Destination)
        }

        pub fn char(&self) -> char {
            match self {
                Self::Destination => 'E',
                Self::Start => 'S',
                Self::NormalSquare { height } => (height + b'a').into(),
            }
        }

        pub fn height(&self) -> u8 {
            match self {
                Self::Destination => 25,
                Self::Start => 0,
                Self::NormalSquare { height } => *height,
            }
        }
    }

    impl TryFrom<char> for Square {
        type Error = ProblemError;

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                'S' => Ok(Square::Start),
                'E' => Ok(Square::Destination),
                c if ('a'..='z').contains(&c) => Ok(Square::NormalSquare {
                    height: c as u8 - b'a',
                }),
                c => Err(ProblemError::InputParseError(format!(
                    "Could not convert char '{c}' into Square"
                ))),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_height() {
            let a: Square = 'a'.try_into().unwrap();
            assert_eq!(a.height(), 0);

            let b: Square = 'b'.try_into().unwrap();
            assert_eq!(b.height(), a.height() + 1);

            let z: Square = 'z'.try_into().unwrap();
            assert_eq!(z.height(), 25);

            let y: Square = 'y'.try_into().unwrap();
            assert_eq!(y.height(), z.height() - 1);

            let start: Square = 'S'.try_into().unwrap();
            assert_eq!(start.height(), a.height());

            let end: Square = 'E'.try_into().unwrap();
            assert_eq!(end.height(), z.height());
        }

        #[test]
        fn test_char_parsing() {
            assert_eq!(
                Square::try_from('a').unwrap(),
                Square::NormalSquare { height: 0 }
            );
            assert_eq!(
                Square::try_from('b').unwrap(),
                Square::NormalSquare { height: 1 }
            );
            assert_eq!(
                Square::try_from('y').unwrap(),
                Square::NormalSquare { height: 24 }
            );
            assert_eq!(
                Square::try_from('z').unwrap(),
                Square::NormalSquare { height: 25 }
            );
            assert_eq!(Square::try_from('S').unwrap(), Square::Start);
            assert_eq!(Square::try_from('E').unwrap(), Square::Destination);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use im::Vector;
    use utils::{
        geom::{CompleteGrid, GridError},
        input::read_lines,
    };

    #[test]
    fn test_part_one() {
        let grid: CompleteGrid<Square> = read_lines("inputs/example.txt")
            .and_then(|lines: Vector<_>| lines.try_into().map_err(|err: GridError| err.into()))
            .unwrap();
        assert_eq!(part_one(&grid).unwrap(), 31);
    }

    #[test]
    fn test_part_two() {
        let grid: CompleteGrid<Square> = read_lines("inputs/example.txt")
            .and_then(|lines: Vector<_>| lines.try_into().map_err(|err: GridError| err.into()))
            .unwrap();
        assert_eq!(part_two(&grid).unwrap(), 29);
    }
}
