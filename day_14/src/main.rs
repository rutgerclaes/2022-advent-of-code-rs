use model::{Cave, Path};
use utils::{geom::Point, input::parse_lines, output::output_success};

fn main() {
    tracing_subscriber::fmt::init();
    let input: Vec<Path> = parse_lines("day_14/inputs/puzzle.txt").unwrap();

    output_success("part 1", part_one(&input));
    output_success("part 2", part_two(&input));
}

fn part_one(input: &[Path]) -> usize {
    let source = Point::new(500, 0);
    let cave = Cave::from(&source, input);

    (0..)
        .scan(cave, |cave, _| cave.insert_sand(&source))
        .count()
}

fn part_two(input: &[Path]) -> usize {
    let source = Point::new(500, 0);
    let cave = Cave::from(&source, input);

    (0..)
        .scan(cave, |cave, _| {
            Some(cave.insert_sand_with_floor(&source)).filter(|rest| rest != &source)
        })
        .count()
        + 1
}

mod model {
    use std::{collections::HashMap, fmt::Display, str::FromStr};

    use itertools::Itertools;
    use tracing::trace;
    use utils::{
        error::ProblemError,
        geom::{BBox, Point},
    };

    #[derive(Debug)]
    pub enum Tile {
        Rock,
        Sand,
    }

    impl Tile {
        pub fn char(&self) -> char {
            match self {
                Self::Rock => '#',
                Self::Sand => 'o',
            }
        }
    }

    #[derive(Debug)]
    pub struct Path(Vec<Point<usize>>);

    impl Path {
        pub fn points(&self) -> impl Iterator<Item = Point<usize>> + '_ {
            self.0.iter().tuple_windows().flat_map(|(start, end)| {
                trace!("Building path between {start} and {end}");
                Path::between(start, end)
            })
        }

        fn between<'a>(
            start: &'a Point<usize>,
            end: &'a Point<usize>,
        ) -> Box<dyn Iterator<Item = Point<usize>> + 'a> {
            if start.x == end.x {
                let (min_y, max_y) = if start.y < end.y {
                    (start.y, end.y)
                } else {
                    (end.y, start.y)
                };
                Box::new((min_y..=max_y).map(|y| Point::new(start.x, y)))
            } else if start.y == end.y {
                let (min_x, max_x) = if start.x < end.x {
                    (start.x, end.x)
                } else {
                    (end.x, start.x)
                };
                Box::new((min_x..=max_x).map(|x| Point::new(x, start.y)))
            } else {
                panic!("Diagonal paths are not supported")
            }
        }
    }

    impl Display for Path {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0.first() {
                None => write!(f, ""),
                Some(p) => {
                    write!(f, "{},{}", p.x, p.y)?;
                    self.0
                        .iter()
                        .skip(1)
                        .try_for_each(|p| write!(f, " -> {},{}", p.x, p.y))?;
                    write!(f, "")
                }
            }
        }
    }

    impl FromStr for Path {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let points = s
                .split(" -> ")
                .map(|s| {
                    if let Some((x, y)) = s
                        .split(',')
                        .map(|i| {
                            i.parse().map_err(|err| {
                                ProblemError::InputParseError(format!(
                                    "Could not parse {i} as int: {err}"
                                ))
                            })
                        })
                        .collect_tuple()
                    {
                        Ok(Point::new(x?, y?))
                    } else {
                        Err(ProblemError::InputParseError(format!(
                            "Could not extract two coordinates from {s}"
                        )))
                    }
                })
                .try_collect()?;

            Ok(Path(points))
        }
    }

    #[derive(Debug)]
    pub struct Cave {
        tiles: HashMap<Point<usize>, Tile>,
        bbox: BBox<usize>,
    }

    impl Cave {
        pub fn from(source: &Point<usize>, paths: &[Path]) -> Self {
            let mut cave = Cave::empty(source);
            paths.iter().for_each(|path| cave.mark_path_as_rock(path));
            cave
        }

        pub fn empty(point: &Point<usize>) -> Self {
            Cave {
                tiles: HashMap::new(),
                bbox: BBox::new(point.x, point.x, point.y, point.y),
            }
        }

        pub fn mark_path_as_rock(&mut self, path: &Path) {
            path.points().for_each(|point| self.mark_as_rock(&point));
        }

        pub fn mark_as_rock(&mut self, pos: &Point<usize>) {
            self.update_bbox(pos);
            self.tiles.insert(*pos, Tile::Rock);
        }

        fn update_bbox(&mut self, pos: &Point<usize>) {
            self.bbox.update(pos);
        }

        pub fn insert_sand(&mut self, pos: &Point<usize>) -> Option<Point<usize>> {
            if pos.y >= self.bbox.max_y() {
                None
            } else {
                let possibilities = [
                    Point::new(pos.x, pos.y + 1),
                    Point::new(pos.x - 1, pos.y + 1),
                    Point::new(pos.x + 1, pos.y + 1),
                ];
                let maybe_next_position =
                    possibilities.iter().find(|p| !self.tiles.contains_key(p));

                if let Some(next_position) = maybe_next_position {
                    trace!("Sand can drop down to {}", next_position);
                    self.insert_sand(next_position)
                } else {
                    trace!("Inserting sand at {}", pos);
                    self.tiles.insert(*pos, Tile::Sand);
                    Some(*pos)
                }
            }
        }

        pub fn insert_sand_with_floor(&mut self, pos: &Point<usize>) -> Point<usize> {
            let possibilities = [
                Point::new(pos.x, pos.y + 1),
                Point::new(pos.x - 1, pos.y + 1),
                Point::new(pos.x + 1, pos.y + 1),
            ];
            let maybe_next_position = possibilities
                .iter()
                .find(|p| !self.tiles.contains_key(p) && p.y < self.floor());

            if let Some(next_position) = maybe_next_position {
                trace!("Sand can drop down to {}", next_position);
                self.insert_sand_with_floor(next_position)
            } else {
                trace!("Inserting sand at {}", pos);
                self.tiles.insert(*pos, Tile::Sand);
                *pos
            }
        }

        pub fn floor(&mut self) -> usize {
            self.bbox.max_y() + 2
        }
    }

    impl Display for Cave {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for y in self.bbox.y_range() {
                write!(f, "{y:04} ")?;
                for x in self.bbox.x_range() {
                    let ch = self
                        .tiles
                        .get(&Point::new(x, y))
                        .map(|t| t.char())
                        .unwrap_or(' ');
                    write!(f, "{ch}")?;
                }
                writeln!(f)?;
            }

            write!(f, "")
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;
    use utils::input::parse_lines;

    use super::*;

    #[test]
    #[traced_test]
    fn test_part_one() {
        let paths: Vec<_> = parse_lines("inputs/example.txt").unwrap();
        assert_eq!(part_one(&paths), 24);
    }

    #[test]
    #[traced_test]
    fn test_part_two() {
        let paths: Vec<_> = parse_lines("inputs/example.txt").unwrap();
        assert_eq!(part_two(&paths), 93);
    }
}
