use model::Instruction;
use tracing::{debug, trace};
use utils::{input::parse_lines, output::output_success};

fn main() {
    tracing_subscriber::fmt::init();
    let instructions: Vec<Instruction> = parse_lines("day_10/inputs/puzzle.txt").unwrap();
    output_success("part 1", part_one(&instructions));

    println!("{}", part_two(&instructions));
}

fn part_one(instructions: &[Instruction]) -> i32 {
    fn evaluate_signal(cycle: i32, value: i32) -> Option<i32> {
        if cycle == 20
            || cycle == 60
            || cycle == 100
            || cycle == 140
            || cycle == 180
            || cycle == 220
        {
            let signal = cycle * value;
            debug!(
                "At start of cycle {}, value is {}, signal is {}",
                cycle, value, signal
            );
            Some(signal)
        } else {
            None
        }
    }

    let (_, _, sum) = instructions
        .iter()
        .fold((1, 1, 0), |(cycle, value, sum), instruction| {
            trace!("handling {} at {}", instruction, cycle);
            match instruction {
                Instruction::NoOp => {
                    trace!("noop: cycle {}: {}", cycle, value);
                    let result = evaluate_signal(cycle, value);

                    (cycle + 1, value, result.unwrap_or(0) + sum)
                }
                Instruction::Add(add_value) => {
                    trace!("addx: cycle {}: {}", cycle, value);
                    let result =
                        evaluate_signal(cycle, value).or_else(|| evaluate_signal(cycle + 1, value));
                    (cycle + 2, value + add_value, result.unwrap_or(0) + sum)
                }
            }
        });

    sum
}

fn part_two(instructions: &[Instruction]) -> String {
    let values = instructions
        .iter()
        .scan((1, 1), |state, instruction| match instruction {
            Instruction::NoOp => {
                let elem: (u32, i32) = (state.0, state.1);
                state.0 += 1;
                Some(vec![elem])
            }
            Instruction::Add(val) => {
                let old_value = state.1;
                let cycle_start = state.0;

                state.0 += 2;
                state.1 += val;

                Some(vec![(cycle_start, old_value), (cycle_start + 1, old_value)])
            }
        })
        .flatten();

    values.fold(String::new(), |mut output, (cycle, pos)| {
        let pixel = (cycle - 1) % 40;

        let pixel_value = if pos.abs_diff(pixel as i32) <= 1 {
            '#'
        } else {
            '.'
        };
        output.push(pixel_value);
        if pixel == 39 {
            output.push('\n');
        }

        output
    })
}

mod model {
    use std::{fmt::Display, str::FromStr};

    use utils::error::ProblemError;

    #[derive(Debug)]
    pub enum Instruction {
        NoOp,
        Add(i32),
    }

    impl Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::NoOp => write!(f, "noop"),
                Self::Add(val) => write!(f, "addx {}", val),
            }
        }
    }

    impl FromStr for Instruction {
        type Err = ProblemError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                _noop if s.starts_with("noop") => Ok(Self::NoOp),
                add if s.starts_with("addx") => s
                    .strip_prefix("addx ")
                    .ok_or(ProblemError::InputParseError(format!(
                        "Could not parse add instruction {} ",
                        add
                    )))
                    .and_then(|s| {
                        s.parse().map(Self::Add).map_err(|err| {
                            ProblemError::InputParseError(format!(
                                "Could not parse value for add: {}",
                                err
                            ))
                        })
                    }),
                other => Err(ProblemError::InputParseError(format!(
                    "Could not parse instruction at {}",
                    other
                ))),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use tracing_test::traced_test;

    use super::*;

    #[test]
    #[traced_test]
    fn test_example_part_one() {
        let instructions: Vec<Instruction> = parse_lines("inputs/example.txt").unwrap();
        let result = part_one(&instructions);
        assert_eq!(result, 13140);
    }

    #[test]
    fn test_example_part_two() {
        let instructions: Vec<Instruction> = parse_lines("inputs/example.txt").unwrap();
        let result = part_two(&instructions);
        let expected = "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"
        .to_owned();
        assert_eq!(result, expected);
    }
}
