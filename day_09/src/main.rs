use im::{hashset, vector, Vector};
use model::Move;
use tracing::debug;
use utils::{geom::Point, input::parse_lines, output::output_success};

fn main() {
    let moves: Vec<Move> = parse_lines("day_09/inputs/puzzle.txt").unwrap();
    output_success("part 1", part_one(&moves));
    output_success("part 2", part_two(&moves));
}

fn part_one(moves: &[Move]) -> usize {
    let head = Point::new(0, 0);
    let tail = Point::new(0, 0);

    let mut positions = hashset![tail];

    (_, _, positions) =
        moves
            .iter()
            .fold((head, tail, positions), |(head, tail, positions), mve| {
                (0..mve.distance).fold((head, tail, positions), |(head, tail, mut positions), _| {
                    let next_head = head.step(&mve.direction);
                    let next_tail = calculate_tail_position(&next_head, tail);

                    positions.insert(next_tail);

                    (next_head, next_tail, positions)
                })
            });

    positions.len()
}

fn part_two(moves: &[Move]) -> usize {
    let head = Point::new(0, 0);
    let rope: Vector<_> = (0..9).map(|_| Point::new(0, 0)).collect();
    let mut positions = hashset![*rope.last().unwrap()];

    (_, _, positions) =
        moves
            .iter()
            .fold((head, rope, positions), |(head, rope, positions), mve| {
                (0..mve.distance).fold((head, rope, positions), |(head, rope, mut positions), _| {
                    let next_head = head.step(&mve.direction);

                    let (_, next_rope) = rope.iter().fold(
                        (next_head, vector![]),
                        |(next_head, mut new_rope), &knot| {
                            let next_knot = calculate_tail_position(&next_head, knot);
                            new_rope.push_back(next_knot);
                            (next_knot, new_rope)
                        },
                    );

                    positions.insert(*next_rope.last().unwrap());

                    (next_head, next_rope, positions)
                })
            });

    positions.len()
}

fn calculate_tail_position(head: &Point<i64>, tail: Point<i64>) -> Point<i64> {
    if head.x.abs_diff(tail.x) <= 1 && head.y.abs_diff(tail.y) <= 1 {
        debug!("H {head} touches T {tail}: not moving");
        tail
    } else {
        debug!("H {head} no longer toches T {tail}");
        let diff_x = head.x - tail.x;
        let diff_y = head.y - tail.y;

        debug!("diff_x: {diff_x}\tdiff_y: {diff_y}");
        Point::new(tail.x + diff_x.signum(), tail.y + diff_y.signum())
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use tracing_test::traced_test;
    use utils::{geom::Point, input::parse_lines};

    use super::*;

    #[test]
    #[traced_test]
    fn test_tail_position_calculation() {
        assert_eq!(
            calculate_tail_position(&Point::new(0, 0), Point::new(0, 0)),
            Point::new(0, 0)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(1, 0), Point::new(0, 0)),
            Point::new(0, 0)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(2, 0), Point::new(0, 0)),
            Point::new(1, 0)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(2, 1), Point::new(1, 0)),
            Point::new(1, 0)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(2, 2), Point::new(1, 0)),
            Point::new(2, 1)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(2, 3), Point::new(2, 1)),
            Point::new(2, 2)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(2, 4), Point::new(2, 2)),
            Point::new(2, 3)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(1, 4), Point::new(2, 3)),
            Point::new(2, 3)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(0, 4), Point::new(2, 3)),
            Point::new(1, 4)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(0, 3), Point::new(1, 4)),
            Point::new(1, 4)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(1, 3), Point::new(1, 4)),
            Point::new(1, 4)
        );
        assert_eq!(
            calculate_tail_position(&Point::new(1, 2), Point::new(1, 4)),
            Point::new(1, 3)
        );
    }

    #[test]
    fn test_part_one() {
        let moves: Vec<Move> = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]
            .iter()
            .map(|s| s.parse())
            .try_collect()
            .unwrap();
        assert_eq!(part_one(&moves), 13);
    }

    #[test]
    fn test_part_two_small() {
        let moves: Vec<Move> = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]
            .iter()
            .map(|s| s.parse())
            .try_collect()
            .unwrap();
        assert_eq!(part_two(&moves), 1);
    }

    #[test]
    fn test_part_two_large() {
        let moves: Vec<Move> = parse_lines("inputs/example.txt").unwrap();
        assert_eq!(part_two(&moves), 36);
    }
}

mod model {
    use itertools::Itertools;
    use std::str::FromStr;
    use utils::{error::ProblemError, geom::Direction};

    #[derive(Debug, Clone, Copy)]
    pub struct Move {
        pub direction: Direction,
        pub distance: usize,
    }

    impl FromStr for Move {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if let Some((dir, dist)) = s.split_ascii_whitespace().collect_tuple() {
                let direction = match dir {
                    "R" => Ok(Direction::Right),
                    "L" => Ok(Direction::Left),
                    "U" => Ok(Direction::Up),
                    "D" => Ok(Direction::Down),
                    val => Err(ProblemError::InputParseError(format!(
                        "Could not parse direction '{val}'"
                    ))),
                }?;

                let distance = dist.parse().map_err(|err| {
                    ProblemError::InputParseError(format!(
                        "Could not parse distance '{dist}': {err}"
                    ))
                })?;

                Ok(Move {
                    direction,
                    distance,
                })
            } else {
                Err(ProblemError::InputParseError(format!(
                    "Could not parse line '{s}'"
                )))
            }
        }
    }
}
