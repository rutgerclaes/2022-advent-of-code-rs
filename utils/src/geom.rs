use std::{
    fmt::Display,
    ops::{Add, Mul, RangeInclusive},
};

use num::{One, Signed};

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
    T: num::Integer + Signed,
{
    pub fn manhattan_distance(&self, other: &Point<T>) -> T {
        self.x.abs_sub(&other.x) + self.y.abs_sub(&other.y)
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

impl<X> Point<X>
where
    X: Copy + One + Add<X, Output = X> + Mul<i32, Output = X>,
{
    pub fn step(&self, direction: &Direction) -> Point<X> {
        let (dx, dy) = direction.delta();
        let x = self.x + X::one() * dx;
        let y = self.y + X::one() * dy;

        Point::new(x, y)
    }

    pub fn direction(&self, direction: &Direction, distance: i32) -> Point<X> {
        let (dx, dy) = direction.delta();
        let x = self.x + X::one() * dx * distance;
        let y = self.y + X::one() * dy * distance;

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
        write!(f, "{}", c)
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
