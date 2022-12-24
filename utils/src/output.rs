use ansi_term::{Color, Style};
use std::{fmt::Display, iter::Step};
use tracing::{event, event_enabled, Level};

pub fn output_success<T>(part: &str, result: T)
where
    T: Display,
{
    if event_enabled!(Level::INFO) {
        event!(Level::INFO, "Solution to {}: {}", part, result)
    } else {
        let formatted_result = Style::new().bold().paint(format!("{result}"));
        println!("Solution to {part}: {formatted_result}");
    }
}

pub fn output<T, E>(part: &str, result: Result<T, E>)
where
    T: Display,
    E: Display,
{
    match result {
        Ok(outcome) => output_success(part, outcome),
        Err(error) if event_enabled!(Level::ERROR) => {
            event!(
                Level::ERROR,
                "Failed to compute solution to {}: {}",
                part,
                error
            )
        }
        Err(error) => {
            let formatted_part = Color::Red.bold().paint(part);
            eprintln!("Failed to compute solution to {formatted_part}: {error}")
        }
    }
}

pub fn grid<T, F, C>(x_start: T, x_end: T, y_start: T, y_end: T, f: F)
where
    F: Fn(&T, &T) -> C,
    C: Display + Sized,
    T: Step + Copy,
{
    (y_start..y_end).for_each(|y| {
        (x_start..x_end).for_each(|x| {
            let output = f(&x, &y);
            print!("{output}")
        });
        println!();
    })
}
