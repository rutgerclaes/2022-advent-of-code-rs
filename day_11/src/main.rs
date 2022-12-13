use anyhow::Result;
use im::{hashmap, vector, HashMap, Vector};
use itertools::Itertools;
use model::Monkey;
use tracing::{debug, trace};
use utils::{error::ProblemError, input::read_lines, output::output};

fn main() {
    tracing_subscriber::fmt::init();

    let lines: Vec<_> = read_lines("day_11/inputs/puzzle.txt").unwrap();
    output("part 1", part_one(&lines));
    output("part 2", part_two(&lines));
}

fn part_one(input: &[String]) -> Result<usize, ProblemError> {
    let (state, monkeys) = Monkey::parse_input(input).unwrap();

    let counts: HashMap<usize, usize> = hashmap! {};

    let (_,_,counts) = (0..20).fold( (state, monkeys,counts), |(state,monkeys, counts),round| {
        debug!( "Starting round {}", round );

        let (updated_state, updated_counts) = monkeys.iter().enumerate().fold( (state, counts), |(state,counts),(idx,monkey)| {
            let items = state.get( &idx ).map( |c| c.to_owned() ).unwrap_or_else( Vector::new );
            let updated_state = state.update( idx, Vector::new() );
            debug!( "Monkey {} (has items {:?}", idx, items );

            items.iter().fold( (updated_state,counts), |(state,counts),item| {
                debug!( "  Monkey inspects an item with a worry level of {}.", item );
                let uc = counts.alter( |existing| Some( existing.unwrap_or( 0 ) + 1 ), idx );
                let updated = monkey.operation.apply(item);
                debug!( "    Worry level is {} to {}", monkey.operation, updated );
                let updated = updated / 3;
                debug!( "    Monkey gets bored iwth item. Worry level is divided by 3 to {}", updated );
                let destination = monkey.destination( &updated );

                debug!( "    Item with worry level {} is thrown to monkey {}", updated, destination );
                let us = state.alter( |existing| {
                    Some( existing.map_or_else( || vector![ updated ], |other| other + vector![ updated ] ) )
                }, destination);


                (us,uc)
            } )
        } );

        ( updated_state,  monkeys, updated_counts )
    } );

    counts
        .values()
        .copied()
        .sorted_by_key(|v| -(*v as i32))
        .take(2)
        .reduce(|a, b| a * b)
        .ok_or(ProblemError::NoSolutionFoundError)
}

fn part_two(input: &[String]) -> Result<usize, ProblemError> {
    let (state, monkeys) = Monkey::parse_input(input).unwrap();

    let counts: HashMap<usize, usize> = hashmap! {};

    let lcm = monkeys
        .iter()
        .map(|m| m.test)
        .reduce(|a, b| a * b)
        .unwrap_or(1);

    let (_, _, counts) = (1..=10_000).fold(
        (state, monkeys, counts),
        |(state, monkeys, counts), round| {
            trace!("Starting round {}", round);

            let (updated_state, updated_counts) = monkeys.iter().enumerate().fold(
                (state, counts),
                |(state, counts), (idx, monkey)| {
                    let items = state
                        .get(&idx)
                        .map(|c| c.to_owned())
                        .unwrap_or_else(Vector::new);
                    let updated_state = state.update(idx, Vector::new());
                    trace!("Monkey {} (has items {:?}", idx, items);

                    items
                        .iter()
                        .fold((updated_state, counts), |(state, counts), item| {
                            trace!("  Monkey inspects an item with a worry level of {}.", item);
                            let uc = counts.alter(|existing| Some(existing.unwrap_or(0) + 1), idx);
                            let updated = monkey.operation.apply(item);
                            trace!("    Worry level is {} to {}", monkey.operation, updated);
                            let updated = updated % lcm;
                            let destination = monkey.destination(&updated);

                            trace!(
                                "    Item with worry level {} is thrown to monkey {}",
                                updated,
                                destination
                            );
                            let us = state.alter(
                                |existing| {
                                    Some(existing.map_or_else(
                                        || vector![updated],
                                        |other| other + vector![updated],
                                    ))
                                },
                                destination,
                            );

                            (us, uc)
                        })
                },
            );

            if round == 1 || round == 20 || round % 1000 == 0 {
                debug!("== After round {} ==", round);
                updated_counts.keys().sorted().for_each(|idx| {
                    debug!(
                        "Monkey {} inspected items {} times",
                        idx,
                        updated_counts.get(idx).unwrap_or(&0)
                    )
                })
            }

            (updated_state, monkeys, updated_counts)
        },
    );

    counts
        .values()
        .copied()
        .sorted_by_key(|v| -(*v as i32))
        .take(2)
        .reduce(|a, b| a * b)
        .ok_or(ProblemError::NoSolutionFoundError)
}

mod model {
    use std::{fmt::Display, slice::Iter, str::FromStr};

    use anyhow::{Context, Result};
    use im::{hashmap, vector, HashMap, Vector};
    use itertools::Itertools;
    use tracing::trace;
    use utils::error::ProblemError;

    #[derive(Copy, Clone, Debug)]
    pub struct MonkeyOperation {
        operand: char,
        rhs: Option<usize>,
    }

    impl MonkeyOperation {
        pub fn apply(&self, lhs: &usize) -> usize {
            match self.operand {
                '+' => lhs + self.rhs.unwrap_or(*lhs),
                _ => lhs * self.rhs.unwrap_or(*lhs),
            }
        }
    }

    impl FromStr for MonkeyOperation {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let pieces: Vec<_> = s.split_ascii_whitespace().collect();
            let operand = match pieces.get(pieces.len() - 2).and_then(|c| c.chars().next()) {
                Some('*') => Ok('*'),
                Some('+') => Ok('+'),
                Some(c) => Err(ProblemError::InputParseError(format!(
                    "Could not parse operation with operand '{}'",
                    c
                ))),
                None => Err(ProblemError::InputParseError(
                    "Could not find operand".to_owned(),
                )),
            }?;

            let rhs: Option<usize> = pieces
                .last()
                .ok_or_else(|| ProblemError::InputParseError("Could not find rhs".to_owned()))
                .map(|number| number.parse().map_or_else(|_| None, Some))?;

            Ok(MonkeyOperation { operand, rhs })
        }
    }

    impl Display for MonkeyOperation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{} {}",
                self.operand,
                self.rhs.map_or("old".to_owned(), |n| format!("{}", n))
            )
        }
    }

    #[derive(Clone, Debug)]
    pub struct Monkey {
        pub operation: MonkeyOperation,
        pub test: usize,
        destinations: (usize, usize),
    }

    impl Monkey {
        pub fn destination(&self, value: &usize) -> usize {
            if value % self.test == 0 {
                self.destinations.0
            } else {
                self.destinations.1
            }
        }

        #[allow(clippy::type_complexity)]
        pub fn parse_input(
            input: &[String],
        ) -> Result<(HashMap<usize, Vector<usize>>, Vector<Monkey>)> {
            fn parse_monkey(it: &mut Iter<String>) -> Result<(Vector<usize>, Monkey)> {
                let current_items: Vector<usize> = it
                    .next()
                    .and_then(|line| line.strip_prefix("  Starting items: "))
                    .ok_or_else(|| {
                        ProblemError::InputParseError("Could not parse starting items".to_owned())
                    })
                    .and_then(|s| {
                        s.split(", ")
                            .map(|e| {
                                e.parse().map_err(|err| {
                                    ProblemError::InputParseError(format!(
                                        "Could not parse starting item '{}': {}",
                                        e, err
                                    ))
                                })
                            })
                            .try_collect()
                    })?;

                let operation: MonkeyOperation = it
                    .next()
                    .and_then(|s| s.strip_prefix("  Operation: new = old "))
                    .ok_or_else(|| {
                        ProblemError::InputParseError(
                            "Could not strip prefix from operation".to_owned(),
                        )
                    })
                    .and_then(|s| s.parse())?;

                let test: usize = it
                    .next()
                    .and_then(|s| s.strip_prefix("  Test: divisible by "))
                    .ok_or_else(|| {
                        ProblemError::InputParseError("Could not strip prefix of test".to_owned())
                    })
                    .and_then(|s| {
                        s.parse().map_err(|err| {
                            ProblemError::InputParseError(format!(
                                "Could not parse test divisor: {}",
                                err
                            ))
                        })
                    })?;

                let when_true = it
                    .next()
                    .and_then(|s| s.strip_prefix("    If true: throw to monkey "))
                    .ok_or_else(|| {
                        ProblemError::InputParseError(
                            "Could not strip prefix of 'if true'".to_owned(),
                        )
                    })
                    .and_then(|s| {
                        s.parse().map_err(|err| {
                            ProblemError::InputParseError(format!(
                                "Could not parse 'if true' destination: {}",
                                err
                            ))
                        })
                    })?;

                let when_false = it
                    .next()
                    .and_then(|s| s.strip_prefix("    If false: throw to monkey "))
                    .ok_or_else(|| {
                        ProblemError::InputParseError(
                            "Could not strip prefix of 'if false'".to_owned(),
                        )
                    })
                    .and_then(|s| {
                        s.parse().map_err(|err| {
                            ProblemError::InputParseError(format!(
                                "Could not parse 'if false' destination: {}",
                                err
                            ))
                        })
                    })?;

                Ok((
                    current_items,
                    Monkey {
                        operation,
                        test,
                        destinations: (when_true, when_false),
                    },
                ))
            }

            let monkeys: Vec<_> = input
                .iter()
                .batching(|it| {
                    if let Some(header) = it.find(|s| !s.is_empty()) {
                        trace!("Starting to parse '{}'", header);
                        Some(parse_monkey(it).context(format!("While parsing {}", header)))
                    } else {
                        None
                    }
                })
                .try_collect()?;

            let res = monkeys.into_iter().enumerate().fold(
                (hashmap! {}, vector![]),
                |(items, mut monkeys), (index, (current_items, current_monkey))| {
                    monkeys.push_back(current_monkey);

                    (items.update(index, current_items), monkeys)
                },
            );

            Ok(res)
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;
    use utils::input::read_lines;

    use super::*;

    #[test]
    #[traced_test]
    fn test_part_one() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let result = part_one(&input).unwrap();

        assert_eq!(result, 10605)
    }

    #[test]
    #[traced_test]
    fn test_part_two() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let result = part_two(&input).unwrap();

        assert_eq!(result, 2713310158)
    }
}
