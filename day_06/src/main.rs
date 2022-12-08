use anyhow::Result;
use im::{hashset, HashSet};
use itertools::{FoldWhile, Itertools};
use tracing::trace;
use utils::{error::ProblemError, input::read_file, output::output};

fn main() {
    tracing_subscriber::fmt::init();
    let input = read_file("day_06/inputs/puzzle.txt").unwrap();

    output("part 1", part_one(&input));
    output("part 2", part_two(&input));
}

fn part_one(input: &str) -> Result<usize, ProblemError> {
    input
        .chars()
        .tuple_windows()
        .enumerate()
        .find(|(_, (a, b, c, d))| a != b && a != c && b != c && a != d && b != d && c != d)
        .map(|(index, _)| index + 4)
        .ok_or(ProblemError::NoSolutionFoundError)
}

fn part_two(input: &str) -> Result<usize, ProblemError> {
    let window_size = 14;
    input
        .chars()
        .enumerate()
        .multipeek()
        .batching(|it| match it.next() {
            Some((position, start)) => {
                trace!("position: {} start: {}", position, start);
                let mut window = (1..window_size).filter_map(|_| it.peek().copied());
                let result = window.fold_while(hashset![start], |seen: HashSet<char>, (_, c)| {
                    if !seen.contains(&c) {
                        trace!("    not yet seen {} ({:?})", c, seen);
                        FoldWhile::Continue(seen.update(c))
                    } else {
                        trace!("    already seen {} ({:?})", c, seen);
                        FoldWhile::Done(seen)
                    }
                });

                match result {
                    FoldWhile::Continue(seen) if seen.len() == window_size => {
                        Some(Some(position + window_size))
                    }
                    _ => Some(None),
                }
            }
            None => None,
        })
        .find_map(|s| s)
        .ok_or(ProblemError::NoSolutionFoundError)
}

#[cfg(test)]
mod tests {

    use tracing_test::traced_test;

    use super::*;

    #[test]
    fn test_part_one() {
        let input = "bvwbjplbgvbhsrlpgdmjqwftvncz";
        assert_eq!(part_one(input).unwrap(), 5);

        let input = "nppdvjthqldpwncqszvftbrmjlhg";
        assert_eq!(part_one(input).unwrap(), 6);

        let input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
        assert_eq!(part_one(input).unwrap(), 10);

        let input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
        assert_eq!(part_one(input).unwrap(), 11);
    }

    #[test]
    #[traced_test]
    fn test_part_two() {
        let input = "abcdefghijklmabcdefghijklmn";
        assert_eq!(part_two(input).unwrap(), 27);

        let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
        assert_eq!(part_two(input).unwrap(), 19);

        let input = "bvwbjplbgvbhsrlpgdmjqwftvncz";
        assert_eq!(part_two(input).unwrap(), 23);
    }
}
