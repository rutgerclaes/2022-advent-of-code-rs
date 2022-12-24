use std::time::SystemTime;

use im::{HashMap, HashSet};
use itertools::Itertools;
use model::{ODMatrix, Valve, ValveId};
use rayon::prelude::*;
use tracing::{info, instrument};
use utils::{input::parse_lines, output::output_success};

fn main() {
    tracing_subscriber::fmt::init();
    let valves: Vec<Valve> = parse_lines("day_16/inputs/puzzle.txt").unwrap();

    let matrix = ODMatrix::build(&valves);
    let flows: HashMap<&ValveId, usize> = valves.iter().map(|v| (&v.id, v.flow)).collect();
    let start: ValveId = "AA".parse().unwrap();

    output_success("part 1", part_one(&start, &valves, &matrix, &flows));
    output_success("part 2", part_two(&start, &valves, &matrix, &flows));
}

#[instrument(skip_all)]
fn part_one<'a>(
    start: &ValveId,
    valves: &'a [Valve],
    matrix: &ODMatrix,
    flows: &HashMap<&'a ValveId, usize>,
) -> i32 {
    let destinations: HashSet<&ValveId> = valves
        .iter()
        .filter(|valve| valve.flow > 0)
        .map(|v| &v.id)
        .collect();
    let start_time = SystemTime::now();
    let result = explore(start, destinations, 30, 0, matrix, flows);
    info!(
        "part one took {:?}",
        SystemTime::now()
            .duration_since(start_time)
            .expect("Time should work")
    );
    result
}

#[instrument(skip_all)]
fn part_two<'a>(
    start: &ValveId,
    valves: &'a [Valve],
    matrix: &ODMatrix,
    flows: &HashMap<&'a ValveId, usize>,
) -> i32 {
    let destinations: HashSet<&ValveId> = valves
        .iter()
        .filter(|valve| valve.flow > 0)
        .map(|v| &v.id)
        .collect();
    let start_time = SystemTime::now();
    let result = explore_next_duo((start, start), destinations, (26, 26), 0, matrix, flows);
    info!(
        "part two took {:?}",
        SystemTime::now()
            .duration_since(start_time)
            .expect("Time should work")
    );
    result
}

fn explore<'a>(
    position: &ValveId,
    not_visited: HashSet<&'a ValveId>,
    ttl: i32,
    pressure: i32,
    matrix: &ODMatrix,
    flows: &HashMap<&'a ValveId, usize>,
) -> i32 {
    not_visited
        .iter()
        .par_bridge()
        .map(|next| {
            let travel_time = *matrix.get(position, next).unwrap() as i32;
            if ttl - travel_time > 0 {
                let time_left_over = ttl - travel_time - 1;
                let pressure_from_valve = *flows.get(next).unwrap_or(&0) as i32 * time_left_over;

                let next_possibilties: HashSet<&ValveId> = not_visited.without(next);
                explore(
                    next,
                    next_possibilties,
                    time_left_over,
                    pressure + pressure_from_valve,
                    matrix,
                    flows,
                )
            } else {
                pressure
            }
        })
        .max()
        .unwrap_or(pressure)
}

fn explore_next_duo<'a>(
    positions: (&ValveId, &ValveId),
    not_visited: HashSet<&'a ValveId>,
    ttl: (i32, i32),
    pressure: i32,
    matrix: &ODMatrix,
    flows: &HashMap<&'a ValveId, usize>,
) -> i32 {
    not_visited
        .iter()
        .permutations(2)
        .par_bridge()
        .map(|next_positions| {
            let next_positions = (
                **next_positions.get(0).unwrap(),
                **next_positions.get(1).unwrap(),
            );

            let travel_times = (
                *matrix.get(positions.0, next_positions.0).unwrap() as i32,
                *matrix.get(positions.1, next_positions.1).unwrap() as i32,
            );

            let next_ttls = (ttl.0 - travel_times.0 - 1, ttl.1 - travel_times.1 - 1);

            let extra_pressures = (
                next_ttls.0
                    * if next_ttls.0 >= 0 {
                        *flows.get(next_positions.0).unwrap_or(&0)
                    } else {
                        0
                    } as i32,
                next_ttls.1
                    * if next_ttls.1 >= 1 {
                        *flows.get(next_positions.1).unwrap_or(&0)
                    } else {
                        0
                    } as i32,
            );

            if next_ttls.0 > 0 && next_ttls.1 > 0 {
                let next_possibilities = not_visited
                    .without(next_positions.0)
                    .without(next_positions.1);
                explore_next_duo(
                    next_positions,
                    next_possibilities,
                    next_ttls,
                    pressure + extra_pressures.0 + extra_pressures.1,
                    matrix,
                    flows,
                )
            } else if next_ttls.0 > 0 {
                let next_possibilities = not_visited
                    .without(next_positions.0)
                    .without(next_positions.1);
                explore(
                    next_positions.0,
                    next_possibilities,
                    next_ttls.0,
                    pressure + extra_pressures.0 + extra_pressures.1,
                    matrix,
                    flows,
                )
            } else if next_ttls.1 > 0 {
                let next_possibilities = not_visited
                    .without(next_positions.0)
                    .without(next_positions.1);
                explore(
                    next_positions.1,
                    next_possibilities,
                    next_ttls.1,
                    pressure + extra_pressures.0 + extra_pressures.1,
                    matrix,
                    flows,
                )
            } else {
                pressure + extra_pressures.0 + extra_pressures.1
            }
        })
        .max()
        .unwrap_or_else(|| {
            if let Some(only_valve_left) = not_visited.iter().next() {
                let travel_times = (
                    *matrix.get(positions.0, only_valve_left).unwrap() as i32,
                    *matrix.get(positions.1, only_valve_left).unwrap() as i32,
                );

                let next_ttls = (ttl.0 - travel_times.0 - 1, ttl.1 - travel_times.1 - 1);

                let extra_pressures = (
                    next_ttls.0
                        * if next_ttls.0 >= 0 {
                            *flows.get(only_valve_left).unwrap_or(&0)
                        } else {
                            0
                        } as i32,
                    next_ttls.1
                        * if next_ttls.1 >= 1 {
                            *flows.get(only_valve_left).unwrap_or(&0)
                        } else {
                            0
                        } as i32,
                );

                if extra_pressures.1 > extra_pressures.0 {
                    pressure + extra_pressures.1
                } else {
                    pressure + extra_pressures.0
                }
            } else {
                pressure
            }
        })
}

mod model {
    use std::{fmt::Display, str::FromStr};

    use im::{hashmap, hashset, HashMap, HashSet};
    use itertools::Itertools;
    use once_cell::unsync::Lazy;
    use regex::Regex;
    use utils::error::ProblemError;

    pub struct ODMatrix<'a>(HashMap<&'a ValveId, HashMap<&'a ValveId, usize>>);

    impl<'a> ODMatrix<'a> {
        pub fn empty() -> Self {
            ODMatrix(HashMap::new())
        }

        pub fn build<I>(valves: I) -> Self
        where
            I: IntoIterator<Item = &'a Valve>,
        {
            let new = Self::empty();
            let lookup: HashMap<&ValveId, &Valve> =
                valves.into_iter().map(|v| (&v.id, v)).collect();

            fn add<'b>(
                matrix: ODMatrix<'b>,
                destination: &'b ValveId,
                distance: usize,
                front: HashSet<&'b ValveId>,
                seen: HashSet<&'b ValveId>,
                lookup: &HashMap<&'b ValveId, &'b Valve>,
            ) -> ODMatrix<'b> {
                if !front.is_empty() {
                    let m = front
                        .iter()
                        .fold(matrix, |m, origin| m.add(origin, destination, distance));

                    let updated_front: HashSet<&ValveId> = front
                        .iter()
                        .flat_map(|o| lookup.get(o).unwrap().tunnels.iter())
                        .filter(|&v| !seen.contains(v))
                        .collect();

                    add(
                        m,
                        destination,
                        distance + 1,
                        updated_front,
                        seen + front,
                        lookup,
                    )
                } else {
                    matrix
                }
            }

            lookup.values().fold(new, |m, destination| {
                add(
                    m,
                    &destination.id,
                    1,
                    destination.tunnels.iter().collect(),
                    hashset! { &destination.id },
                    &lookup,
                )
            })
        }

        pub fn get(&self, origin: &ValveId, destination: &ValveId) -> Option<&usize> {
            self.0
                .get(origin)
                .and_then(|destinations| destinations.get(destination))
        }

        fn add(&self, origin: &'a ValveId, destination: &'a ValveId, distance: usize) -> Self {
            let updated = self.0.alter(
                |destinations| match destinations {
                    None => Some(hashmap! { destination => distance }),
                    Some(dest) => Some(dest.alter(
                        |existing_distance| {
                            existing_distance
                                .filter(|d| d <= &distance)
                                .or(Some(distance))
                        },
                        destination,
                    )),
                },
                origin,
            );
            ODMatrix(updated)
        }
    }

    impl Display for ODMatrix<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut entries: Vec<_> = self.0.keys().collect();
            entries.sort();

            entries.iter().try_for_each(|i| write!(f, "\t{i}"))?;
            writeln!(f)?;

            entries.iter().try_for_each(|o| {
                write!(f, "{o:}")?;
                entries.iter().try_for_each(|&d| {
                    let distance = self
                        .get(o, d)
                        .map(|d| format!("{d}"))
                        .unwrap_or_else(|| "NA".to_owned());

                    write!(f, "\t{distance}")
                })?;
                writeln!(f)
            })
        }
    }

    #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct ValveId(char, char);

    impl Display for ValveId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.0, self.1)
        }
    }

    impl FromStr for ValveId {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (a, b) = s
                .chars()
                .collect_tuple()
                .ok_or_else(|| ProblemError::InputParseError(format!("Invalid valve id: '{s}'")))?;
            Ok(ValveId(a, b))
        }
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    pub struct Valve {
        pub id: ValveId,
        pub flow: usize,
        pub tunnels: HashSet<ValveId>,
    }

    impl Display for Valve {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "Valve {} has flow rate={}; tunnels lead to valves {:?}",
                self.id, self.flow, self.tunnels
            )
        }
    }

    impl FromStr for Valve {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let regex: Lazy<Regex> = Lazy::new({
                || {
                    Regex::new( "^Valve (?P<id>[A-Z]+) has flow rate=(?P<flow>[0-9]+); tunnel[s]? lead[s]? to valve[s]? (?P<tunnels>[A-Z]+(, [A-Z]+)*)$").unwrap()
                }
            });

            match regex.captures(s) {
                Some(captures) => {
                    let id: ValveId = captures
                        .name("id")
                        .ok_or_else(|| {
                            ProblemError::InputParseError(format!("Could not find id in {s}"))
                        })?
                        .as_str()
                        .parse()?;
                    let flow: usize = captures
                        .name("flow")
                        .ok_or_else(|| {
                            ProblemError::InputParseError(format!("Could not find flow in {s}"))
                        })?
                        .as_str()
                        .parse()?;
                    let tunnels: HashSet<ValveId> = captures
                        .name("tunnels")
                        .ok_or_else(|| {
                            ProblemError::InputParseError(format!("Could not find tunnels in {s}"))
                        })?
                        .as_str()
                        .split(", ")
                        .map(|s| s.parse())
                        .try_collect()?;

                    Ok(Valve { id, flow, tunnels })
                }
                None => Err(ProblemError::InputParseError(format!(
                    "Regex didn't match input '{s}'"
                ))),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use im::hashset;

        use super::*;

        #[test]
        #[allow(non_snake_case)]
        fn test_od_matrix() {
            let A = ValveId('A', 'A');
            let B = ValveId('B', 'A');
            let C = ValveId('C', 'A');
            let D = ValveId('D', 'A');
            let E = ValveId('E', 'A');
            let F = ValveId('F', 'A');

            let valves = [
                Valve {
                    id: A,
                    flow: 0,
                    tunnels: hashset! { B, F },
                },
                Valve {
                    id: B,
                    flow: 0,
                    tunnels: hashset! { A, C },
                },
                Valve {
                    id: C,
                    flow: 0,
                    tunnels: hashset! { B, D },
                },
                Valve {
                    id: D,
                    flow: 0,
                    tunnels: hashset! { C, F, E },
                },
                Valve {
                    id: E,
                    flow: 0,
                    tunnels: hashset! { F, D },
                },
                Valve {
                    id: F,
                    flow: 0,
                    tunnels: hashset! { A, D, E },
                },
            ];

            let matrix = ODMatrix::build(&valves);

            assert_eq!(matrix.get(&A, &B), Some(&1));
            assert_eq!(matrix.get(&A, &C), Some(&2));
            assert_eq!(matrix.get(&A, &D), Some(&2));
            assert_eq!(matrix.get(&A, &E), Some(&2));
            assert_eq!(matrix.get(&A, &F), Some(&1));

            assert_eq!(matrix.get(&B, &A), Some(&1));
            assert_eq!(matrix.get(&B, &F), Some(&2));
            assert_eq!(matrix.get(&B, &E), Some(&3));
            assert_eq!(matrix.get(&B, &C), Some(&1));
            assert_eq!(matrix.get(&B, &D), Some(&2));

            assert_eq!(matrix.get(&F, &A), Some(&1));
            assert_eq!(matrix.get(&E, &A), Some(&2));
            assert_eq!(matrix.get(&D, &A), Some(&2));
            assert_eq!(matrix.get(&C, &A), Some(&2));
            assert_eq!(matrix.get(&B, &A), Some(&1));
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_part_one() {
        let valves: Vec<Valve> = parse_lines("inputs/example.txt").unwrap();

        let matrix = ODMatrix::build(&valves);
        let flows: HashMap<&ValveId, usize> = valves.iter().map(|v| (&v.id, v.flow)).collect();
        let start: ValveId = "AA".parse().unwrap();

        assert_eq!(part_one(&start, &valves, &matrix, &flows), 1651);
    }

    #[test]
    fn test_part_two() {
        let valves: Vec<Valve> = parse_lines("inputs/example.txt").unwrap();

        let matrix = ODMatrix::build(&valves);
        let flows: HashMap<&ValveId, usize> = valves.iter().map(|v| (&v.id, v.flow)).collect();
        let start: ValveId = "AA".parse().unwrap();

        assert_eq!(part_two(&start, &valves, &matrix, &flows), 1707);
    }
}
