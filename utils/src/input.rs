use anyhow::{Context, Result};
use std::{fs::File, io::BufRead, io::BufReader, str::FromStr};
use tracing::debug_span;

pub fn parse_lines<I, T>(path: &str) -> Result<I>
where
    I: FromIterator<T>,
    T: FromStr,
    <T as FromStr>::Err: std::error::Error + Sync + Send + 'static,
{
    let _guard = debug_span!("input::parse_lines", path = path).entered();
    let file =
        File::open(path).context(format!("Could not open input file '{path}' for reading"))?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .enumerate()
        .map(|(i, line)| -> Result<T> {
            let string_value = line?;
            let value = string_value
                .parse()
                .context(format!("Could not parse input line {i}: '{string_value}'"))?;
            Ok(value)
        })
        .try_collect()
}

pub fn read_file(path: &str) -> Result<String> {
    let _guard = debug_span!("input::read_file", path = path).entered();
    let file_content = std::fs::read_to_string(path)
        .context(format!("Could not open input file '{path}' for reading"))?;
    Ok(file_content)
}

pub fn read_lines<I>(path: &str) -> Result<I>
where
    I: FromIterator<String>,
{
    let _guard = debug_span!("input::read_lines", path = path).entered();
    let file =
        File::open(path).context(format!("Could not open input file '{path}' for reading"))?;
    let reader = BufReader::new(file);

    let result: I = reader.lines().try_collect()?;
    Ok(result)
}

pub fn parse_file<T>(path: &str) -> Result<T>
where
    T: FromStr,
    <T as FromStr>::Err: std::error::Error + Sync + Send + 'static,
{
    let _guard = debug_span!("input::parse_file", path = path).entered();
    let result = read_file(path)?
        .parse::<T>()
        .context(format!("Could not parse the input file '{path}'"))?;
    Ok(result)
}
