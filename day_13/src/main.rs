#![feature(option_result_contains)]
use std::cmp::Ordering;

use itertools::Itertools;
use model::PacketData;
use tracing::debug;
use utils::{error::ProblemError, input::read_lines, output::output};

fn main() {
    tracing_subscriber::fmt::init();
    let input: Vec<_> = read_lines("day_13/inputs/puzzle.txt").unwrap();
    output("part 1", part_one(&input));
    output("part 2", part_two(&input));
}

fn part_one(input: &[String]) -> Result<usize, ProblemError> {
    let pairs: Vec<(PacketData, PacketData)> = input
        .iter()
        .batching(|it| {
            let first = it.next().map(|s| s.parse())?;
            let second = it.next().map(|s| s.parse())?;

            it.next(); // get rid of empty line

            Some(first.and_then(|f| second.map(|s| (f, s))))
        })
        .try_collect()
        .map_err(|err| ProblemError::InputParseError(format!("Error parsing input: {err}")))?;

    pairs
        .iter()
        .enumerate()
        .filter_map(|(idx, (lhs, rhs))| match lhs.partial_cmp(rhs) {
            Some(Ordering::Less) => {
                debug!("{idx}: in order (Ordering::Less) {lhs} - {rhs}");
                Some(idx + 1)
            }
            Some(Ordering::Greater) => {
                debug!("{idx}: not in order (Ordering::Greater) {lhs} - {rhs}");
                None
            }
            Some(Ordering::Equal) => {
                debug!("{idx}: not in order (Ordering::Equal) {lhs} - {rhs}");
                None
            }
            None => {
                debug!("{idx}: not in order (None) {lhs} - {rhs}");
                None
            }
        })
        .reduce(|a, b| a + b)
        .ok_or(ProblemError::NoSolutionFoundError)
}

fn part_two(input: &[String]) -> Result<usize, ProblemError> {
    let messages: Vec<PacketData> = input
        .iter()
        .filter(|line| !line.is_empty())
        .map(|l| {
            l.parse().map_err(|err| {
                ProblemError::InputParseError(format!("Could not parse '{l}': {err}"))
            })
        })
        .try_collect()?;

    let marker_a = PacketData::single_list(PacketData::single_list(PacketData::value(2)));
    let marker_b = PacketData::single_list(PacketData::single_list(PacketData::value(6)));

    let markers = vec![marker_a, marker_b];

    let sorted: Vec<_> = markers.iter().chain(messages.iter()).sorted().collect();

    sorted
        .iter()
        .enumerate()
        .filter_map(|(idx, elem)| {
            if markers.contains(elem) {
                Some(idx + 1)
            } else {
                None
            }
        })
        .reduce(|a, b| a * b)
        .ok_or(ProblemError::NoSolutionFoundError)
}

mod model {
    use std::{cmp::Ordering, fmt::Display, str::FromStr};

    use anyhow::Result;
    use im::Vector;
    use len_trait::{Empty, Len};
    use thiserror::Error;
    use tracing::{error, trace};

    #[derive(Debug)]
    pub enum PacketData {
        Value(u8),
        List(PacketDataList),
    }

    impl Display for PacketData {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Value(val) => write!(f, "{val}"),
                Self::List(list) => write!(f, "[{list}]"),
            }
        }
    }

    #[derive(Error, Debug)]
    pub enum PacketDataError {
        #[error("Error parsing packet data: {0}")]
        PacketDataParseError(String),
    }

    impl PacketData {
        pub fn value(i: u8) -> PacketData {
            PacketData::Value(i)
        }

        pub fn list(list: PacketDataList) -> PacketData {
            PacketData::List(list)
        }

        #[allow(dead_code)]
        pub fn from_iter<I, D>(input: I) -> PacketData
        where
            I: IntoIterator<Item = D>,
            D: Into<PacketData>,
        {
            Self::list(input.into_iter().collect())
        }

        #[allow(dead_code)]
        pub fn single_list<D>(item: D) -> PacketData
        where
            D: Into<PacketData>,
        {
            Self::list(PacketDataList::single(item.into()))
        }

        #[allow(dead_code)]
        pub fn empty_list() -> PacketData {
            Self::list(PacketDataList::empty())
        }

        pub fn parse(input: &str) -> Result<PacketData, PacketDataError> {
            let (result, tail) = Self::read_data(input.chars().collect())?;
            if tail.is_empty() {
                Ok(result)
            } else {
                Err(PacketDataError::PacketDataParseError(format!(
                    "Leftover after parsing PacketData: {tail:?}"
                )))
            }
        }

        fn read_data(input: Vector<char>) -> Result<(PacketData, Vector<char>), PacketDataError> {
            match input.head() {
                None => {
                    error!("No characters in input while reading data");
                    Err(PacketDataError::PacketDataParseError(
                        "Trying to read data from empty list of chars".to_owned(),
                    ))
                }
                Some('[') => {
                    trace!("Encountered list start while reading data");
                    let (list, tail) = Self::read_list(input.skip(1))?;
                    let stripped_data = if tail.head().map_or_else(|| false, |&c| c == ',') {
                        tail.skip(1)
                    } else {
                        tail
                    };
                    Ok((PacketData::List(list), stripped_data))
                }
                Some(_) => {
                    let count = input.iter().take_while(|c| c.is_numeric()).count();
                    let (data, tail) = input.split_at(count);
                    let packet_data: PacketData = data
                        .iter()
                        .fold(0, |d, i| d * 10 + i.to_digit(10).unwrap() as u8)
                        .into();

                    let stripped_data = if tail.head().map_or_else(|| false, |&c| c == ',') {
                        tail.skip(1)
                    } else {
                        tail
                    };

                    Ok((packet_data, stripped_data))
                }
            }
        }

        fn read_list(
            input: Vector<char>,
        ) -> Result<(PacketDataList, Vector<char>), PacketDataError> {
            match input.head() {
                Some(']') => {
                    trace!("Encountered list end, returning empty list");
                    Ok((PacketDataList::empty(), input.skip(1)))
                }
                Some(_) => {
                    trace!("Recursively reading list");
                    let (next, input_tail) = Self::read_data(input)?;
                    let (data_tail, input_tail) = Self::read_list(input_tail)?;

                    Ok((
                        PacketDataList::Cons(Box::new(next), Box::new(data_tail)),
                        input_tail,
                    ))
                }
                None => {
                    error!("No characters in input while reading list");
                    Err(PacketDataError::PacketDataParseError(
                        "Trying to read list from empty list of chars".to_owned(),
                    ))
                }
            }
        }
    }

    #[derive(Debug)]
    pub enum PacketDataList {
        Nil,
        Cons(Box<PacketData>, Box<PacketDataList>),
    }

    impl PacketDataList {
        fn empty() -> Self {
            Self::Nil
        }

        fn single(elem: PacketData) -> Self {
            Self::Cons(Box::new(elem), Box::new(Self::Nil))
        }

        fn append(self, elem: PacketData) -> Self {
            match self {
                Self::Nil => Self::single(elem),
                Self::Cons(head, tail) => Self::Cons(head, Box::new(tail.append(elem))),
            }
        }
    }

    impl Display for PacketDataList {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Nil => write!(f, ""),
                Self::Cons(head, tail) if !tail.is_empty() => write!(f, "{head},{tail}"),
                Self::Cons(head, _) => write!(f, "{head}"),
            }
        }
    }

    impl Empty for PacketDataList {
        fn is_empty(&self) -> bool {
            matches!(self, Self::Nil)
        }
    }

    impl Len for PacketDataList {
        fn len(&self) -> usize {
            match self {
                Self::Nil => 0,
                Self::Cons(_, tail) => tail.len() + 1,
            }
        }
    }

    impl From<u8> for PacketData {
        fn from(value: u8) -> Self {
            PacketData::value(value)
        }
    }

    impl From<PacketDataList> for PacketData {
        fn from(value: PacketDataList) -> Self {
            PacketData::list(value)
        }
    }

    impl PartialEq for PacketData {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Value(l0), Self::Value(r0)) => l0 == r0,
                (Self::List(l0), Self::List(r0)) => l0 == r0,
                _ => false,
            }
        }
    }

    impl PartialOrd for PacketData {
        #[allow(clippy::comparison_chain)] // we want to be explicit about the comparison
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            trace!("Compare {} vs {}", self, other);
            match (self, other) {
                (Self::Value(self_value), Self::Value(other_value)) => {
                    if self_value < other_value {
                        trace!("Left side is smaller, so inputs are in the right order");
                        Some(Ordering::Less)
                    } else if self_value > other_value {
                        trace!("Right side is smaller, so inputs are not in the right order");
                        Some(Ordering::Greater)
                    } else {
                        None
                    }
                }
                (Self::List(self_list), Self::List(other_list)) => {
                    self_list.partial_cmp(other_list)
                }
                (Self::Value(val), Self::List(other)) => {
                    trace!("Mixed types; convert left to list and retry comparison");
                    PacketDataList::single(Self::Value(*val)).partial_cmp(other)
                }
                (Self::List(self_list), Self::Value(val)) => {
                    trace!("Mixed types; convert right to list and retry comparison");
                    self_list.partial_cmp(&PacketDataList::single(Self::Value(*val)))
                }
            }
        }
    }

    impl Ord for PacketData {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap_or(Ordering::Equal)
        }
    }

    impl Eq for PacketData {}

    impl PartialEq for PacketDataList {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Nil, Self::Nil) => true,
                (Self::Cons(l0, l1), Self::Cons(r0, r1)) => l0 == r0 && l1 == r1,
                _ => false,
            }
        }
    }

    impl PartialOrd for PacketDataList {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            match (self, other) {
                (Self::Nil, Self::Nil) => None,
                (Self::Nil, _) => {
                    trace!("Left side ran out of items, so inputs are in right order");
                    Some(Ordering::Less)
                }
                (_, Self::Nil) => {
                    trace!("Right side ran out of items so inputs are no in the right order");
                    Some(Ordering::Greater)
                }
                (Self::Cons(head, tail), Self::Cons(other_head, other_tail)) => head
                    .partial_cmp(other_head)
                    .or_else(|| tail.partial_cmp(other_tail)),
            }
        }
    }

    impl<D> FromIterator<D> for PacketData
    where
        D: Into<PacketData>,
    {
        fn from_iter<T: IntoIterator<Item = D>>(iter: T) -> Self {
            let inner: PacketDataList = iter.into_iter().collect();
            Self::List(inner)
        }
    }

    impl<D> FromIterator<D> for PacketDataList
    where
        D: Into<PacketData>,
    {
        fn from_iter<T: IntoIterator<Item = D>>(iter: T) -> Self {
            iter.into_iter()
                .fold(Self::Nil, |list, i| list.append(i.into()))
        }
    }

    impl FromStr for PacketData {
        type Err = PacketDataError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            PacketData::parse(s)
        }
    }

    #[cfg(test)]
    mod tests {
        use tracing_test::traced_test;

        use super::*;

        #[test]
        #[traced_test]
        fn test_part_one_first_example() {
            let lhs: PacketDataList = [1, 1, 3, 1, 1].into_iter().collect();
            let rhs: PacketDataList = [1, 1, 5, 1, 1].into_iter().collect();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Less));

            let lhs: PacketData = PacketData::List(lhs);
            let rhs: PacketData = PacketData::List(rhs);

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Less));
        }

        #[test]
        #[traced_test]
        fn test_part_one_second_example() {
            let lhs: PacketData = "[[1],[2,3,4]]".parse().unwrap();
            let rhs: PacketData = "[[1],4]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Less))
        }

        #[test]
        #[traced_test]
        fn test_part_one_third_example() {
            let lhs: PacketData = "[9]".parse().unwrap();
            let rhs: PacketData = "[[8,7,6]]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_part_one_fourth_example() {
            let lhs: PacketData = "[[4,4],4,4]".parse().unwrap();
            let rhs: PacketData = "[[4,4],4,4,4]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Less))
        }

        #[test]
        #[traced_test]
        fn test_part_one_fifth_example() {
            let lhs: PacketData = "[7,7,7,7]".parse().unwrap();
            let rhs: PacketData = "[7,7,7]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_part_one_sixth_example() {
            let lhs: PacketData = "[]".parse().unwrap();
            let rhs: PacketData = "[3]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Less))
        }

        #[test]
        #[traced_test]
        fn test_part_one_seventh_example() {
            let lhs: PacketData = "[[[]]]".parse().unwrap();
            let rhs: PacketData = "[[]]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_part_one_eight_example() {
            let lhs: PacketData = "[1,[2,[3,[4,[5,6,7]]]],8,9]".parse().unwrap();
            let rhs: PacketData = "[1,[2,[3,[4,[5,6,0]]]],8,9]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_sample_107() {
            let lhs: PacketData = "[[],[[[1,0,9],[1,4,1,0],[1,3],5,[9,10,5,1,0]],9,[[7,8,9],[6,5,0,8,6],10,[9,8,8],10],[8,4,[]],[]]]".parse().unwrap();
            let rhs: PacketData = "[[]]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_sample_116() {
            let lhs: PacketData = "[[],[[6,[6,3,0,9,10],2,7,0]],[[[6,4,0,0,7],1,0],8]]"
                .parse()
                .unwrap();
            let rhs: PacketData = "[[],[0,5,[5,9,10,[6,0,1,0,5]],[[6,5,6,5]],[[0,1]]],[[[],8,[]],[[2,2],2,[1,4,6,9],[],10],5,6,[[3,0],4,7,[3,6,9]]],[[9]]]".parse().unwrap();

            assert_eq!(lhs.partial_cmp(&rhs), Some(Ordering::Greater))
        }

        #[test]
        #[traced_test]
        fn test_parsing() {
            let data: PacketData = "10".parse().unwrap();
            assert_eq!(data, PacketData::value(10));

            let data: PacketData = "[]".parse().unwrap();
            assert_eq!(data, PacketData::empty_list());

            let data: PacketData = "[1,1,3,1,1]".parse().unwrap();
            assert_eq!(data, [1, 1, 3, 1, 1].into_iter().collect());

            let data: PacketData = "[[1],[2,3,4]]".parse().unwrap();
            let a: PacketData = PacketData::single_list(1);
            let b: PacketData = [2, 3, 4].into_iter().collect();
            let expected: PacketData = [a, b].into_iter().collect();
            assert_eq!(data, expected);

            let data: PacketData = "[[[]]]".parse().unwrap();
            let expected =
                PacketData::single_list(PacketData::single_list(PacketData::empty_list()));
            assert_eq!(data, expected);
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
    fn test_example_part_one() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let result = part_one(&input).unwrap();
        assert_eq!(result, 13)
    }

    #[test]
    #[traced_test]
    fn test_example_part_two() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let result = part_two(&input).unwrap();
        assert_eq!(result, 140)
    }
}
