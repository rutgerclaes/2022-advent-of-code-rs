#![feature(iterator_try_collect)]

use im::{vector, HashSet, Vector};
use itertools::Itertools;
use once_cell::unsync::Lazy;
use regex::Regex;
use utils::{
    error::ProblemError,
    geom::Point,
    input::read_lines,
    output::{output, output_success},
};

fn main() {
    let lines: Vec<_> = read_lines("day_15/inputs/puzzle.txt").unwrap();
    let y = 2000000;

    let sensors: Vec<_> = lines.iter().map(parse_line).try_collect().unwrap();
    output_success("part 1", part_one(y, &sensors));
    output("part 2", part_two(&sensors, y * 2));
}

fn part_one(row_index: i32, sensors: &[(Point<i32>, Point<i32>)]) -> i32 {
    let mut ranges = vector![];

    ranges = sensors
        .iter()
        .filter_map(|(sensor, beacon)| {
            Some(project(
                &row_index,
                sensor,
                &sensor.manhattan_distance(beacon),
            ))
            .filter(|(a, b)| a != b)
        })
        .fold(ranges, |mut ranges, range| {
            merge(&mut ranges, range);
            ranges
        });

    let beacons_positions: HashSet<i32> = sensors
        .iter()
        .filter_map(|(_, b)| if b.y == row_index { Some(b.x) } else { None })
        .collect();
    let sum_ranges = ranges.iter().fold(0, |sum, (s, e)| sum + 1 + (e - s));
    sum_ranges - beacons_positions.len() as i32
}

fn part_two(sensors: &[(Point<i32>, Point<i32>)], max_range: i32) -> Result<i64, ProblemError> {
    let y_ranges = sensors
        .iter()
        .map(|(sensor, beacon)| {
            let distance = sensor.manhattan_distance(beacon) + 1;
            (
                (sensor.y - distance).clamp(0, max_range),
                (sensor.y + distance).clamp(0, max_range),
            )
        })
        .fold(vector![], |mut ranges, range| {
            merge(&mut ranges, range);
            ranges
        });

    let pos = y_ranges.iter().flat_map(|&(a, b)| a..=b).find_map(|y| {
        let ranges = sensors
            .iter()
            .filter_map(|(sensor, beacon)| {
                Some(project(&y, sensor, &sensor.manhattan_distance(beacon)))
                    .filter(|(a, b)| a != b)
            })
            .fold(vector![], |mut ranges, range| {
                merge(&mut ranges, range);
                ranges
            });

        ranges
            .iter()
            .find_map(|&(a, b)| {
                if a > 0 && a < max_range + 1 {
                    Some(a - 1)
                } else if b > -1 && b < max_range {
                    Some(b + 1)
                } else {
                    None
                }
            })
            .map(|x| (x, y))
    });

    pos.map(|(x, y)| x as i64 * 4000000 + y as i64)
        .ok_or(ProblemError::NoSolutionFoundError)
}

fn project(row: &i32, point: &Point<i32>, radius: &i32) -> (i32, i32) {
    let width = *radius - (point.y - row).abs();
    if width < 0 {
        (point.x, point.x)
    } else {
        (point.x - width, point.x + width)
    }
}

fn merge(ranges: &mut Vector<(i32, i32)>, range: (i32, i32)) {
    let first_overlap = ranges.iter().enumerate().find_map(|(i, elem)| {
        if elem.1 >= range.0 - 1 {
            Some((i, elem))
        } else {
            None
        }
    });
    let last_overlap = ranges.iter().enumerate().rev().find_map(|(i, elem)| {
        if elem.0 <= range.1 + 1 {
            Some((i, elem))
        } else {
            None
        }
    });

    let cut_start = first_overlap.map(|(i, _)| i).unwrap_or(ranges.len());
    let cut_end = last_overlap.map(|(i, _)| i + 1).unwrap_or(0);
    let cut = ranges.slice(cut_start..cut_end);
    let entry = (
        cut.head()
            .map(|&(a, _)| a)
            .filter(|&a| a < range.0)
            .unwrap_or(range.0),
        cut.last()
            .map(|&(_, b)| b)
            .filter(|&b| b > range.1)
            .unwrap_or(range.1),
    );
    ranges.insert(cut_start, entry);
}

fn parse_line(line: &String) -> Result<(Point<i32>, Point<i32>), ProblemError> {
    let regex = Lazy::new(|| {
        Regex::new("^Sensor at x=(?P<sensor_x>[0-9-]+), y=(?P<sensor_y>[0-9-]+): closest beacon is at x=(?P<beacon_x>[0-9-]+), y=(?P<beacon_y>[0-9-]+)$" ).unwrap()
    });

    match regex.captures(line) {
        Some(captures) => {
            let sensor_x: i32 = captures
                .name("sensor_x")
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!("No sensor_x value found in '{line}'"))
                })
                .and_then(|s| {
                    s.as_str().parse().map_err(|err| {
                        ProblemError::InputParseError(format!(
                            "Could not parse {} as integer: {err}",
                            s.as_str()
                        ))
                    })
                })?;
            let sensor_y: i32 = captures
                .name("sensor_y")
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!("No sensor_y value found in '{line}'"))
                })
                .and_then(|s| {
                    s.as_str().parse().map_err(|err| {
                        ProblemError::InputParseError(format!(
                            "Could not parse {} as integer: {err}",
                            s.as_str()
                        ))
                    })
                })?;
            let beacon_x: i32 = captures
                .name("beacon_x")
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!("No beacon_x value found in '{line}'"))
                })
                .and_then(|s| {
                    s.as_str().parse().map_err(|err| {
                        ProblemError::InputParseError(format!(
                            "Could not parse {} as integer: {err}",
                            s.as_str()
                        ))
                    })
                })?;
            let beacon_y: i32 = captures
                .name("beacon_y")
                .ok_or_else(|| {
                    ProblemError::InputParseError(format!("No beacon_y value found in '{line}'"))
                })
                .and_then(|s| {
                    s.as_str().parse().map_err(|err| {
                        ProblemError::InputParseError(format!(
                            "Could not parse {} as integer: {err}",
                            s.as_str()
                        ))
                    })
                })?;

            let sensor_location = Point::new(sensor_x, sensor_y);
            let beacon_location = Point::new(beacon_x, beacon_y);

            Ok((sensor_location, beacon_location))
        }
        None => Err(ProblemError::InputParseError(format!(
            "Line '{line}' didn't match regex"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use im::{vector, Vector};
    use tracing_test::traced_test;
    use utils::geom::Point;

    use super::*;

    #[test]
    #[traced_test]
    fn test_small_example() {
        let sensor = Point::new(8, 7);
        let beacon = Point::new(2, 10);

        let radius = sensor.manhattan_distance(&beacon);
        let range = project(&10, &sensor, &radius);

        assert_eq!(range, (2, 14));
    }

    #[test]
    #[traced_test]
    fn test_part_one_example() {
        let lines: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let sensors: Vec<_> = lines.iter().map(parse_line).try_collect().unwrap();
        let result = part_one(10, &sensors);

        assert_eq!(result, 26);
    }

    #[test]
    #[traced_test]
    fn test_merge() {
        let input = vector![(0, 5), (10, 15), (20, 25), (30, 35)];

        fn test_merge(input: &Vector<(i32, i32)>, range: (i32, i32)) -> Vector<(i32, i32)> {
            let mut copy = input.into_iter().copied().collect();
            merge(&mut copy, range);
            copy
        }

        assert_eq!(
            test_merge(&input, (12, 22)),
            vector![(0, 5), (10, 25), (30, 35)]
        );
        assert_eq!(
            test_merge(&input, (17, 18)),
            vector![(0, 5), (10, 15), (17, 18), (20, 25), (30, 35)]
        );
        assert_eq!(
            test_merge(&input, (16, 19)),
            vector![(0, 5), (10, 25), (30, 35)]
        );

        assert_eq!(
            test_merge(&input, (12, 18)),
            vector![(0, 5), (10, 18), (20, 25), (30, 35)]
        );
        assert_eq!(
            test_merge(&input, (17, 22)),
            vector![(0, 5), (10, 15), (17, 25), (30, 35)]
        );

        assert_eq!(
            test_merge(&input, (7, 27)),
            vector![(0, 5), (7, 27), (30, 35)]
        );

        assert_eq!(
            test_merge(&input, (-3, -2)),
            vector![(-3, -2), (0, 5), (10, 15), (20, 25), (30, 35)]
        );
        assert_eq!(
            test_merge(&input, (-3, -1)),
            vector![(-3, 5), (10, 15), (20, 25), (30, 35)]
        );
        assert_eq!(
            test_merge(&input, (37, 38)),
            vector![(0, 5), (10, 15), (20, 25), (30, 35), (37, 38)]
        );
        assert_eq!(
            test_merge(&input, (36, 38)),
            vector![(0, 5), (10, 15), (20, 25), (30, 38)]
        );

        assert_eq!(test_merge(&vector![], (3, 4)), vector![(3, 4)]);
        assert_eq!(test_merge(&vector![(3, 4)], (2, 7)), vector![(2, 7)]);
        assert_eq!(
            test_merge(&vector![(3, 4)], (0, 1)),
            vector![(0, 1), (3, 4)]
        );
        assert_eq!(
            test_merge(&vector![(3, 4)], (6, 8)),
            vector![(3, 4), (6, 8)]
        );
    }
}
