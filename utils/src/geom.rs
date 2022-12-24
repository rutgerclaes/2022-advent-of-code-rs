use std::{
    fmt::{Debug, Display},
    ops::{Add, Mul, RangeInclusive},
};

use im::Vector;
use itertools::Itertools;
use num::{abs, One, Signed};
use thiserror::Error;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct BBox<T> {
    min_x: T,
    max_x: T,
    min_y: T,
    max_y: T,
}

impl<T> BBox<T>
where
    T: PartialOrd,
{
    pub fn new(x1: T, x2: T, y1: T, y2: T) -> BBox<T> {
        let (min_x, max_x) = if x1 <= x2 { (x1, x2) } else { (x2, x1) };
        let (min_y, max_y) = if y1 <= y2 { (y1, y2) } else { (y2, y1) };
        BBox {
            min_x,
            max_x,
            min_y,
            max_y,
        }
    }

    pub fn contains(&self, point: &Point<T>) -> bool {
        point.x >= self.min_x
            && point.x <= self.max_x
            && point.y <= self.max_y
            && point.y >= self.min_y
    }
}

impl<T> BBox<T>
where
    T: Copy,
{
    pub fn x_range(&self) -> RangeInclusive<T> {
        self.min_x..=self.max_x
    }

    pub fn y_range(&self) -> RangeInclusive<T> {
        self.min_y..=self.max_y
    }

    pub fn min_y(&self) -> T {
        self.min_y
    }

    pub fn min_x(&self) -> T {
        self.min_x
    }

    pub fn max_y(&self) -> T {
        self.max_y
    }

    pub fn max_x(&self) -> T {
        self.max_x
    }
}

impl<T> BBox<T>
where
    T: Copy + PartialOrd,
{
    pub fn update(&mut self, point: &Point<T>) {
        if self.min_x >= point.x {
            self.min_x = point.x;
        }
        if self.max_x <= point.x {
            self.max_x = point.x;
        }
        if self.min_y >= point.y {
            self.min_y = point.y;
        }
        if self.max_y <= point.y {
            self.max_y = point.y;
        }
    }
}

impl<C> FromIterator<Point<C>> for Option<BBox<C>>
where
    C: PartialOrd + Copy,
{
    fn from_iter<T: IntoIterator<Item = Point<C>>>(iter: T) -> Self {
        let bounds: Option<(C, C, C, C)> =
            iter.into_iter().fold(None, |bounds, point| match bounds {
                Some((c_min_x, c_max_x, c_min_y, c_max_y)) => {
                    let min_x = if point.x < c_min_x { point.x } else { c_min_x };
                    let max_x = if point.x > c_max_x { point.x } else { c_max_x };
                    let min_y = if point.y < c_min_y { point.y } else { c_min_y };
                    let max_y = if point.y > c_max_y { point.y } else { c_max_y };

                    Some((min_x, max_x, min_y, max_y))
                }
                _ => Some((point.x, point.x, point.y, point.y)),
            });

        bounds.map(|(min_x, max_x, min_y, max_y)| BBox {
            min_x,
            max_x,
            min_y,
            max_y,
        })
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
}

impl<T> Point<T> {
    pub fn new(x: T, y: T) -> Point<T> {
        Point { x, y }
    }
}

impl<T> Point<T>
where
    T: num::Integer + Signed + Copy,
{
    pub fn manhattan_distance(&self, other: &Point<T>) -> T {
        abs(self.x - other.x) + abs(self.y - other.y)
    }
}

impl<X, Y> Add<Point<X>> for Point<Y>
where
    Y: Add<X, Output = Y>,
{
    type Output = Point<Y>;

    fn add(self, rhs: Point<X>) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<X, Y> Add<&Point<X>> for Point<Y>
where
    Y: Add<X, Output = Y>,
    X: Copy,
{
    type Output = Point<Y>;

    fn add(self, rhs: &Point<X>) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

// impl<X> Point<X>
// where
//     X: Copy + One + Add<X, Output = X> + Mul<i32, Output = X>,
// {
//     pub fn step(&self, direction: &Direction) -> Point<X> {
//         let (dx, dy) = direction.delta();
//         let x = self.x + X::one() * dx;
//         let y = self.y + X::one() * dy;

//         Point::new(x, y)
//     }

//     pub fn direction(&self, direction: &Direction, distance: i32) -> Point<X> {
//         let (dx, dy) = direction.delta();
//         let x = self.x + X::one() * dx * distance;
//         let y = self.y + X::one() * dy * distance;

//         Point::new(x, y)
//     }
// }

impl<X> Point<X>
where
    X: Copy + One + Add<X, Output = X> + Mul<i64, Output = X>,
{
    pub fn step(&self, direction: &Direction) -> Point<X> {
        let (dx, dy) = direction.delta();
        let x = self.x + X::one() * dx as i64;
        let y = self.y + X::one() * dy as i64;

        Point::new(x, y)
    }

    pub fn direction(&self, direction: &Direction, distance: i64) -> Point<X> {
        let (dx, dy) = direction.delta();
        let x = self.x + X::one() * dx as i64 * distance;
        let y = self.y + X::one() * dy as i64 * distance;

        Point::new(x, y)
    }
}

impl<S, R> From<(S, S)> for Point<R>
where
    R: From<S>,
{
    fn from(value: (S, S)) -> Self {
        Point {
            x: value.0.into(),
            y: value.1.into(),
        }
    }
}

impl<S, R> From<Point<S>> for (R, R)
where
    R: From<S>,
{
    fn from(value: Point<S>) -> Self {
        (value.x.into(), value.y.into())
    }
}

impl<C> Display for Point<C>
where
    C: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Direction {
    Up,
    Right,
    Left,
    Down,
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::Down => "D",
            Self::Left => "L",
            Self::Up => "U",
            Self::Right => "R",
        };
        write!(f, "{c}")
    }
}

impl Direction {
    pub fn delta(&self) -> (i32, i32) {
        match self {
            Self::Up => (0, 1),
            Self::Down => (0, -1),
            Self::Left => (-1, 0),
            Self::Right => (1, 0),
        }
    }

    pub fn opposite(&self) -> Direction {
        match self {
            Self::Up => Direction::Down,
            Self::Down => Direction::Up,
            Self::Left => Direction::Right,
            Self::Right => Direction::Left,
        }
    }

    pub fn left(&self) -> Direction {
        match self {
            Self::Up => Direction::Left,
            Self::Left => Direction::Down,
            Self::Down => Direction::Right,
            Self::Right => Direction::Up,
        }
    }

    pub fn right(&self) -> Direction {
        match self {
            Self::Up => Direction::Right,
            Self::Right => Direction::Down,
            Self::Down => Direction::Left,
            Self::Left => Direction::Up,
        }
    }

    pub fn rotate(&self, rotation: &Rotation) -> Direction {
        match rotation {
            Rotation::Left => self.left(),
            Rotation::Right => self.right(),
        }
    }
}

pub enum Rotation {
    Left,
    Right,
}

pub struct CompleteGrid<C>(Vector<Vector<C>>);

#[derive(Debug, Error)]
pub enum GridError {
    #[error("Irregular grid")]
    IrregularGrid,

    #[error("Error building grid: {0}")]
    BuildError(String),
}

impl<C> CompleteGrid<C>
where
    C: Copy,
{
    pub fn build(elements: Vector<Vector<C>>) -> Result<CompleteGrid<C>, GridError> {
        let regular = elements
            .iter()
            .tuple_windows()
            .all(|(a, b)| a.len() == b.len());
        if regular {
            Ok(CompleteGrid(elements))
        } else {
            Err(GridError::IrregularGrid)
        }
    }

    #[allow(dead_code)]
    pub fn get(&self, point: &Point<usize>) -> Option<&C> {
        self.0.get(point.y).and_then(|row| row.get(point.x))
    }

    pub fn row(&self, y: usize) -> Option<&Vector<C>> {
        self.0.get(y)
    }

    pub fn width(&self) -> usize {
        self.0.head().map(|r| r.len()).unwrap_or(0)
    }

    #[allow(dead_code)]
    pub fn height(&self) -> usize {
        self.0.len()
    }

    pub fn col(&self, x: usize) -> Option<Vector<&C>> {
        if x >= self.width() {
            None
        } else {
            let col: Vector<&C> = self.0.iter().map(|r| r.get(x).unwrap()).collect();
            Some(col)
        }
    }

    #[allow(dead_code)]
    pub fn bbox(&self) -> Option<BBox<usize>> {
        if self.height() >= 1 && self.width() >= 1 {
            Some(BBox::new(0, 0, self.width() - 1, self.height() - 1))
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Point<usize>, &C)> {
        self.0.iter().enumerate().flat_map(move |(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, elem)| (Point::new(x, y), elem))
        })
    }

    pub fn neighbours<'a, I>(&'a self, loc: &Point<usize>) -> I
    where
        I: FromIterator<(Point<usize>, &'a C)>,
    {
        [
            (loc.x as i64, loc.y as i64 + 1),
            (loc.x as i64, loc.y as i64 - 1),
            (loc.x as i64 + 1, loc.y as i64),
            (loc.x as i64 - 1, loc.y as i64),
        ]
        .into_iter()
        .filter_map(|(x, y)| {
            if x < 0 || y < 0 || x >= self.width() as i64 || y >= self.height() as i64 {
                None
            } else {
                let point = Point::new(x as usize, y as usize);
                self.get(&point).map(|e| (point, e))
            }
        })
        .collect()
    }

    #[allow(dead_code)]
    pub fn map<F, O>(&self, f: F) -> CompleteGrid<O>
    where
        F: Fn(Point<usize>, &C) -> O,
        O: Clone,
    {
        let values: Vector<Vector<O>> = self
            .0
            .iter()
            .enumerate()
            .map(move |(y, row)| {
                let new_row: Vector<O> = row
                    .iter()
                    .enumerate()
                    .map(|(x, elem)| {
                        let point = Point::new(x, y);
                        f(point, elem)
                    })
                    .collect();

                new_row
            })
            .collect();

        CompleteGrid(values)
    }
}

impl<C> TryFrom<Vector<Vector<C>>> for CompleteGrid<C>
where
    C: Copy,
{
    type Error = GridError;

    fn try_from(value: Vector<Vector<C>>) -> Result<Self, Self::Error> {
        CompleteGrid::build(value)
    }
}

impl<C> TryFrom<Vector<String>> for CompleteGrid<C>
where
    C: TryFrom<char> + Copy,
    C::Error: Debug,
{
    type Error = GridError;

    fn try_from(value: Vector<String>) -> Result<Self, Self::Error> {
        let elements: Vector<Vector<C>> = value
            .into_iter()
            .map(|s| {
                s.chars()
                    .map(|c| {
                        c.try_into()
                            .map_err(|err: C::Error| GridError::BuildError(format!("{err:?}")))
                    })
                    .try_collect()
            })
            .try_collect()?;
        CompleteGrid::build(elements)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_manhattan_distance() {
        let a = Point::new(0, 0);
        let x: i32 = a.x;

        println!("dx {}", x.abs_diff(2i32));
        println!("dx {}", x.abs_diff(-2i32));

        assert_eq!(a.manhattan_distance(&Point::new(0, 2)), 2);
        assert_eq!(a.manhattan_distance(&Point::new(2, 0)), 2);
        assert_eq!(a.manhattan_distance(&Point::new(2, 2)), 4);
        assert_eq!(a.manhattan_distance(&Point::new(0, -2)), 2);
        assert_eq!(a.manhattan_distance(&Point::new(-2, 0)), 2);
        assert_eq!(a.manhattan_distance(&Point::new(-2, -2)), 4);
        assert_eq!(a.manhattan_distance(&Point::new(-2, 2)), 4);
        assert_eq!(a.manhattan_distance(&Point::new(2, -2)), 4);

        let a = Point::new(3, 3);

        assert_eq!(a.manhattan_distance(&Point::new(3, 3)), 0);
        assert_eq!(a.manhattan_distance(&Point::new(3, 0)), 3);
        assert_eq!(a.manhattan_distance(&Point::new(0, 3)), 3);
        assert_eq!(a.manhattan_distance(&Point::new(6, 3)), 3);
        assert_eq!(a.manhattan_distance(&Point::new(3, 6)), 3);
        assert_eq!(a.manhattan_distance(&Point::new(6, 6)), 6);
        assert_eq!(a.manhattan_distance(&Point::new(0, 0)), 6);
    }
}
