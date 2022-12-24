use std::num::ParseIntError;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ProblemError {
    #[error("No solution found")]
    NoSolutionFoundError,

    #[error("An assumption about the input doesn't hold: {0}.")]
    InputAssumptionFailedError(&'static str),

    #[error("Error parsing problem input: {0}")]
    InputParseError(String),
}

impl From<ParseIntError> for ProblemError {
    fn from(value: ParseIntError) -> Self {
        ProblemError::InputParseError(format!("Error parsing integer: {value}"))
    }
}
