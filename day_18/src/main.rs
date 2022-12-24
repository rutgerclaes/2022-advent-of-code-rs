use im::{hashset, HashMap, HashSet};

use model::{Cube, Point, Space};
use tailcall::tailcall;
use utils::{
    error::ProblemError,
    input::parse_lines,
    output::{output, output_success},
};

fn main() {
    tracing_subscriber::fmt::init();
    let cubes: Vec<Cube> = parse_lines("day_18/inputs/puzzle.txt").unwrap();

    output_success("part 1", part_one(&cubes));
    output("part 2", part_two(&cubes));
}

fn part_one(cubes: &[Cube]) -> usize {
    let counts = cubes.iter().fold(HashMap::new(), |counts, cube| {
        cube.edge_positions()
            .into_iter()
            .fold(counts, |counts, edge| {
                counts.update_with(edge, 1, |a, b| a + b)
            })
    });

    counts.iter().filter(|(_, &count)| count == 1).count()
}

fn part_two(cubes: &[Cube]) -> Result<usize, ProblemError> {
    let occupied: HashSet<Point> = cubes.iter().map(|c| c.center()).collect();
    let space = Space::from_points(occupied.iter()).ok_or(ProblemError::NoSolutionFoundError)?;
    let grown_space = space.grow();
    let front: HashSet<Point> = grown_space.boundary::<HashSet<_>>();
    let reachable = hashset! {};

    #[tailcall]
    fn expand(
        front: HashSet<Point>,
        reachable: HashSet<Point>,
        filled: &HashSet<Point>,
        space: &Space,
    ) -> HashSet<Point> {
        if front.is_empty() {
            reachable
        } else {
            let mut new_points: HashSet<Point> = front
                .iter()
                .flat_map(|p| p.neighbours().into_iter())
                .collect();

            let new_reachable = reachable.union(front);

            new_points
                .retain(|p| space.contains(p) && !new_reachable.contains(p) && !filled.contains(p));

            expand(new_points, new_reachable, filled, space)
        }
    }

    let filled: Vec<_> = expand(front, reachable, &occupied, &space)
        .into_iter()
        .map(Cube::new)
        .collect();

    let surface = part_one(&filled);
    let outer_surface = grown_space.surface();

    Ok(surface - outer_surface)
}

mod model {
    use std::{iter::once, num::ParseIntError, str::FromStr};

    use itertools::Itertools;
    use utils::error::ProblemError;

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct Space {
        min_x: i64,
        max_x: i64,
        min_y: i64,
        max_y: i64,
        min_z: i64,
        max_z: i64,
    }

    impl Space {
        fn new(p: &Point) -> Self {
            Space {
                min_x: p.0,
                max_x: p.0,
                min_y: p.1,
                max_y: p.1,
                min_z: p.2,
                max_z: p.2,
            }
        }

        pub fn from_points<'a>(points: impl Iterator<Item = &'a Point>) -> Option<Self> {
            points.fold(None, |space, point| {
                if let Some(space) = space {
                    Some(space.expand(point))
                } else {
                    Some(Space::new(point))
                }
            })
        }

        pub fn height(&self) -> usize {
            (self.max_z - self.min_z + 1) as usize
        }

        pub fn width(&self) -> usize {
            (self.max_y - self.min_y + 1) as usize
        }

        pub fn depth(&self) -> usize {
            (self.max_x - self.min_x + 1) as usize
        }

        fn expand(&self, point: &Point) -> Self {
            Space {
                min_x: if point.0 < self.min_x {
                    point.0
                } else {
                    self.min_x
                },
                max_x: if point.0 > self.max_x {
                    point.0
                } else {
                    self.max_x
                },
                min_y: if point.1 < self.min_y {
                    point.1
                } else {
                    self.min_y
                },
                max_y: if point.1 > self.max_y {
                    point.1
                } else {
                    self.max_y
                },
                min_z: if point.2 < self.min_z {
                    point.2
                } else {
                    self.min_z
                },
                max_z: if point.2 > self.max_z {
                    point.2
                } else {
                    self.max_z
                },
            }
        }

        pub fn grow(&self) -> Self {
            Space {
                min_x: self.min_x - 1,
                max_x: self.max_x + 1,
                min_y: self.min_y - 1,
                max_y: self.max_y + 1,
                min_z: self.min_z - 1,
                max_z: self.max_z + 1,
            }
        }

        pub fn contains(&self, point: &Point) -> bool {
            point.0 >= self.min_x
                && point.0 <= self.max_x
                && point.1 >= self.min_y
                && point.1 <= self.max_y
                && point.2 >= self.min_z
                && point.2 <= self.max_z
        }

        pub fn boundary<C>(&self) -> C
        where
            C: FromIterator<Point>,
        {
            let z_planes = ((self.min_x)..=(self.max_x)).flat_map(|x| {
                ((self.min_y)..=(self.max_y)).flat_map(move |y| {
                    once(Point::new(x, y, self.min_z)).chain(once(Point::new(x, y, self.max_z)))
                })
            });
            let x_planes = ((self.min_z)..=(self.max_z)).flat_map(|z| {
                ((self.min_y)..=(self.max_y)).flat_map(move |y| {
                    once(Point::new(self.min_x, y, z)).chain(once(Point::new(self.max_x, y, z)))
                })
            });
            let y_planes = ((self.min_x)..=(self.max_x)).flat_map(|x| {
                ((self.min_z)..=(self.max_z)).flat_map(move |z| {
                    once(Point::new(x, self.min_y, z)).chain(once(Point::new(x, self.max_y, z)))
                })
            });

            z_planes.chain(x_planes).chain(y_planes).collect()
        }

        pub fn surface(&self) -> usize {
            self.height() * self.width() * 2
                + self.height() * self.depth() * 2
                + self.width() * self.depth() * 2
        }
    }

    #[derive(Debug, Hash, Eq, Clone, Copy, PartialEq)]
    pub struct Point(i64, i64, i64);

    #[derive(Debug, Hash, Eq, Clone, Copy, PartialEq)]
    pub struct Edge(i64, i64, i64);

    impl Point {
        pub fn new(x: i64, y: i64, z: i64) -> Self {
            Point(x, y, z)
        }

        pub fn neighbours(&self) -> [Point; 6] {
            [
                Point(self.0 - 1, self.1, self.2),
                Point(self.0 + 1, self.1, self.2),
                Point(self.0, self.1 - 1, self.2),
                Point(self.0, self.1 + 1, self.2),
                Point(self.0, self.1, self.2 - 1),
                Point(self.0, self.1, self.2 + 1),
            ]
        }
    }

    impl FromStr for Point {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let coords: Vec<i64> = s
                .split(',')
                .into_iter()
                .map(|p| {
                    p.parse()
                        .map_err(|e: ParseIntError| -> ProblemError { e.into() })
                })
                .try_collect()?;
            let (x, y, z) = coords.into_iter().collect_tuple().ok_or_else(|| {
                ProblemError::InputParseError(format!("Could not extract coordinates from '{s}'"))
            })?;
            Ok(Point(x, y, z))
        }
    }
    pub struct Cube(Point);

    impl Cube {
        pub fn new(p: Point) -> Self {
            Cube(p)
        }

        pub fn center(&self) -> Point {
            self.0
        }

        pub fn edge_positions(&self) -> [Edge; 6] {
            [
                Edge(2 * self.0 .0, 2 * self.0 .1, 2 * self.0 .2 - 1),
                Edge(2 * self.0 .0, 2 * self.0 .1, 2 * self.0 .2 + 1),
                Edge(2 * self.0 .0, 2 * self.0 .1 - 1, 2 * self.0 .2),
                Edge(2 * self.0 .0, 2 * self.0 .1 + 1, 2 * self.0 .2),
                Edge(2 * self.0 .0 - 1, 2 * self.0 .1, 2 * self.0 .2),
                Edge(2 * self.0 .0 + 1, 2 * self.0 .1, 2 * self.0 .2),
            ]
        }
    }

    impl FromStr for Cube {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            s.parse().map(Cube::new)
        }
    }

    #[cfg(test)]
    mod tests {

        use im::HashSet;

        use super::*;

        #[test]
        fn test_spaces() {
            let points: HashSet<Point> = (1..=3)
                .flat_map(|x| (1..=3).flat_map(move |y| (1..=3).map(move |z| Point::new(x, y, z))))
                .collect();
            let space = Space::from_points(points.iter()).unwrap();

            assert_eq!(space.width(), 3);
            assert_eq!(space.height(), 3);
            assert_eq!(space.depth(), 3);

            assert_eq!(space.surface(), 3 * 3 * 6);

            let boundary: HashSet<Point> = space.boundary();
            assert_eq!(boundary.len(), 3 * 3 * 3 - 1);
            assert_eq!(
                boundary.len(),
                space.surface()
                    - 4 * (space.width() - 1)
                    - 4 * (space.height() - 1)
                    - 4 * (space.depth() - 1)
                    - 4
            );
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_part_one() {
        let cubes: Vec<Cube> = parse_lines("inputs/example.txt").unwrap();
        let result = part_one(&cubes);
        assert_eq!(result, 64);
    }

    #[test]
    fn test_part_two() {
        let cubes: Vec<Cube> = parse_lines("inputs/example.txt").unwrap();
        let result = part_two(&cubes).unwrap();
        assert_eq!(result, 58);
    }
}
