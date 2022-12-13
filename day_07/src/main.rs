use anyhow::{anyhow, Result};
use itertools::Itertools;
use model::*;
use tracing::{info, warn};
use utils::{
    error::ProblemError,
    input::read_lines,
    output::{output, output_success},
};

fn main() {
    tracing_subscriber::fmt::init();

    let lines: Vec<_> = read_lines("day_07/inputs/puzzle.txt").unwrap();
    let root = parse_lines(&lines).unwrap();
    output_success("part 1", part_one(&root));
    output("part 2", part_two(&root));
}

fn part_one(root: &FileSystemNode) -> usize {
    root.iter()
        .filter(|n| n.is_dir() && n.size() < 100000)
        .map(|f| f.size())
        .sum()
}

fn part_two(root: &FileSystemNode) -> Result<usize, ProblemError> {
    let free_space = 70000000 - root.size();
    info!("free space is {}", free_space);
    let cleanup: i64 = 30000000i64 - free_space as i64;
    info!("need to cleanup {}", cleanup);

    if cleanup >= 0 {
        root.iter()
            .filter(|n| n.is_dir() && n.size() as i64 > cleanup)
            .map(|d| d.size())
            .min()
            .ok_or(ProblemError::NoSolutionFoundError)
    } else {
        warn!("No cleanup needed");
        Ok(0)
    }
}

fn parse_lines(input: &[String]) -> Result<FileSystemNode> {
    let mut root = FileSystemNode::root([]);
    let stack = vec![];

    let mut commands = input.iter().batching(|it| match it.next() {
        None => None,
        Some(cmd) => match cmd.parse::<Command>() {
            Ok(Command::List) => {
                let output: Vec<_> = it
                    .take_while_ref(|line| !line.starts_with("$ "))
                    .cloned()
                    .collect();
                Some(Ok((Command::List, Some(output))))
            }
            Ok(command) => Some(Ok((command, None))),
            Err(err) => Some(Err(err)),
        },
    });

    let result: Result<Result<Vec<_>>, _> =
        commands.fold_ok(Ok(stack), |stack, command| match command {
            (Command::ToRoot, _) => Ok(vec![]),
            (Command::GoUp, _) => {
                let mut s = stack?;
                if s.is_empty() {
                    Err(
                        FileSystemError::InvalidMove(String::from("Trying to go above root"))
                            .into(),
                    )
                } else {
                    s.truncate(s.len() - 1);
                    Ok(s)
                }
            }
            (Command::List, Some(entries)) => {
                let s = stack?;
                let current = root.resolve_mut(&s).unwrap();

                let result: Result<_> = entries.iter().try_fold(current, |node, entry| {
                    if let Some((first, last)) = entry.split_ascii_whitespace().collect_tuple() {
                        if first == "dir" {
                            node.mkdir(&[last.to_owned()])?;
                        } else {
                            let file = FileSystemNode::file(last, first.parse::<usize>()?);
                            node.insert(file);
                        }

                        Ok(node)
                    } else {
                        Err(anyhow!("Failed to parse instruction: {}", entry))
                    }
                });

                result?;
                Ok(s)
            }
            (Command::List, None) => stack,
            (Command::GoDown(name), _) => {
                let mut s = stack?;
                s.push(name);
                root.mkdir(&s)?;
                Ok(s)
            }
        });

    result??;

    Ok(root)
}

mod model {
    use std::{iter::Once, str::FromStr};

    use im::{ordmap::Values, OrdMap};
    use thiserror::Error;

    pub enum Command {
        List,
        GoUp,
        GoDown(String),
        ToRoot,
    }

    #[derive(Debug, Error)]
    pub enum CommandError {
        #[error("Error parsing: '{0}'")]
        ParseError(String),
    }

    impl FromStr for Command {
        type Err = CommandError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if s == "$ ls" {
                Ok(Self::List)
            } else if s == "$ cd .." {
                Ok(Self::GoUp)
            } else if s == "$ cd /" {
                Ok(Self::ToRoot)
            } else if s.starts_with("$ cd ") {
                let name = s.strip_prefix("$ cd ").unwrap();
                Ok(Self::GoDown(name.to_owned()))
            } else {
                Err(CommandError::ParseError(format!(
                    "Error parsing {} as command",
                    s
                )))
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum FileSystemNode {
        Directory {
            name: String,
            files: OrdMap<String, FileSystemNode>,
        },

        File {
            name: String,
            size: usize,
        },
    }

    impl FileSystemNode {
        pub fn is_file(&self) -> bool {
            match self {
                Self::File { .. } => true,
                Self::Directory { .. } => false,
            }
        }

        pub fn is_dir(&self) -> bool {
            !self.is_file()
        }

        pub fn name(&self) -> &String {
            match self {
                Self::Directory { name, .. } => name,
                Self::File { name, .. } => name,
            }
        }

        pub fn size(&self) -> usize {
            match self {
                Self::File { size, .. } => *size,
                Self::Directory { files, .. } => {
                    files.values().fold(0, |sum, node| sum + node.size())
                }
            }
        }

        pub fn empty_directory(name: &str) -> FileSystemNode {
            FileSystemNode::Directory {
                name: name.to_owned(),
                files: OrdMap::new(),
            }
        }

        pub fn directory<I>(name: &str, children: I) -> FileSystemNode
        where
            I: IntoIterator<Item = FileSystemNode>,
        {
            FileSystemNode::Directory {
                name: String::from(name),
                files: Self::create_map(children),
            }
        }

        pub fn root<I>(children: I) -> FileSystemNode
        where
            I: IntoIterator<Item = FileSystemNode>,
        {
            Self::directory("", children)
        }

        pub fn file(name: &str, size: usize) -> FileSystemNode {
            Self::File {
                name: String::from(name),
                size,
            }
        }

        pub fn mkdir(&mut self, stack: &[String]) -> Result<&FileSystemNode, FileSystemError> {
            if let Some((first, tail)) = stack.split_first() {
                match self {
                    Self::File { .. } => Err(FileSystemError::DirectoryOperationOnFile(
                        "Trying to mkdir on file".to_owned(),
                    )),
                    Self::Directory { files, .. } => {
                        if !files.contains_key(first) {
                            let new = FileSystemNode::empty_directory(first);
                            files.insert(first.to_owned(), new);
                        }
                        let existing = files.get_mut(first).unwrap();
                        existing.mkdir(tail)
                    }
                }
            } else {
                Ok(self)
            }
        }

        #[allow(dead_code)]
        pub fn resolve(&self, stack: &[String]) -> Result<&FileSystemNode, FileSystemError> {
            if let Some((first, tail)) = stack.split_first() {
                match self {
                    Self::File { .. } => Err(FileSystemError::DirectoryOperationOnFile(
                        "Trying to resolve in to file".to_owned(),
                    )),
                    Self::Directory { files, .. } => files
                        .get(first)
                        .ok_or_else(|| FileSystemError::FileNotFound(vec![first.to_owned()]))
                        .and_then(|child| child.resolve(tail)),
                }
            } else {
                Ok(self)
            }
        }

        pub fn resolve_mut(
            &mut self,
            stack: &[String],
        ) -> Result<&mut FileSystemNode, FileSystemError> {
            if let Some((first, tail)) = stack.split_first() {
                match self {
                    Self::File { .. } => Err(FileSystemError::DirectoryOperationOnFile(
                        "Trying to resolve in to file".to_owned(),
                    )),
                    Self::Directory { files, .. } => files
                        .get_mut(first)
                        .ok_or_else(|| FileSystemError::FileNotFound(vec![first.to_owned()]))
                        .and_then(|child| child.resolve_mut(tail)),
                }
            } else {
                Ok(self)
            }
        }

        // pub fn get( &self, name: &str ) -> Option<&FileSystemNode> {
        //     match self {
        //         Self::File { .. } => None,
        //         Self::Directory { files, .. } => files.get( name ),
        //     }
        // }

        pub fn insert(&mut self, node: FileSystemNode) {
            match self {
                Self::File { .. } => panic!("Error inserting node into a file"),
                Self::Directory { files, .. } => {
                    files.insert(node.name().to_owned(), node);
                }
            }
        }

        fn create_map<I>(nodes: I) -> OrdMap<String, FileSystemNode>
        where
            I: IntoIterator<Item = FileSystemNode>,
        {
            nodes
                .into_iter()
                .map(|node| (node.name().to_owned(), node))
                .collect()
        }

        fn child_nodes(&self) -> ChildIterator {
            ChildIterator::new(self)
        }

        pub fn iter(&self) -> RecursiveIterator<'_> {
            RecursiveIterator::new(self)
        }

        #[allow(dead_code)]
        pub fn tree(node: &FileSystemNode) -> String {
            fn sub_tree(indent: &str, node: &FileSystemNode) -> String {
                match node {
                    FileSystemNode::File { name, size } => {
                        format!("{}- {} (file, size={})\n", indent, name, size)
                    }
                    FileSystemNode::Directory { name, files } => {
                        let next: String = files
                            .values()
                            .map(|node| sub_tree(&(String::from(indent) + "  "), node))
                            .collect();
                        format!("{}- {} (dir)\n{}", indent, name, next)
                    }
                }
            }

            sub_tree("", node)
        }
    }

    #[derive(Debug, Error)]
    pub enum FileSystemError {
        #[error("Directory operation on file: {0}")]
        DirectoryOperationOnFile(String),

        #[error("File not found: {0:?}")]
        FileNotFound(Vec<String>),

        #[error("{0}")]
        InvalidMove(String),
    }

    pub struct RecursiveIterator<'a> {
        root: Once<&'a FileSystemNode>,
        children: ChildIterator<'a>,
        current: Option<Box<RecursiveIterator<'a>>>,
    }

    impl RecursiveIterator<'_> {
        fn new(root: &FileSystemNode) -> RecursiveIterator<'_> {
            RecursiveIterator {
                root: std::iter::once(root),
                children: root.child_nodes(),
                current: None,
            }
        }
    }

    impl<'a> Iterator for RecursiveIterator<'a> {
        type Item = &'a FileSystemNode;

        fn next(&mut self) -> Option<Self::Item> {
            match self.root.next() {
                Some(root) => Some(root),
                None => match self.current.as_mut() {
                    None => {
                        if let Some(next_child) = self.children.next() {
                            self.current = Some(Box::new(next_child.iter()));
                            self.next()
                        } else {
                            None
                        }
                    }
                    Some(boxed_current) => match boxed_current.next() {
                        Some(res) => Some(res),
                        None => {
                            self.current = None;
                            self.next()
                        }
                    },
                },
            }
        }
    }

    impl<'a> Iterator for ChildIterator<'a> {
        type Item = &'a FileSystemNode;

        fn next(&mut self) -> Option<Self::Item> {
            self.underlying.as_mut().and_then(|i| i.next())
        }
    }

    struct ChildIterator<'a> {
        underlying: Option<Values<'a, String, FileSystemNode>>,
    }

    impl ChildIterator<'_> {
        fn empty() -> ChildIterator<'static> {
            ChildIterator { underlying: None }
        }

        fn new(node: &FileSystemNode) -> ChildIterator<'_> {
            match node {
                FileSystemNode::File { .. } => ChildIterator::empty(),
                FileSystemNode::Directory { files, .. } => ChildIterator {
                    underlying: Some(files.values()),
                },
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::FileSystemNode;

        #[test]
        fn test_recursive_iterator() {
            let c = FileSystemNode::empty_directory("c");
            let b = FileSystemNode::directory("b", [c]);
            let a = FileSystemNode::directory("a", [b]);

            let f = FileSystemNode::file("f", 123);
            let e = FileSystemNode::empty_directory("e");
            let d = FileSystemNode::directory("d", [e, f]);

            let g = FileSystemNode::file("g", 123);

            let root = FileSystemNode::root([a, d, g]);

            let names: Vec<_> = root.iter().map(|n| n.name()).collect();

            assert_eq!(names, vec!["", "a", "b", "c", "d", "e", "f", "g"]);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_parsing() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let root = parse_lines(&input).unwrap();

        println!("{}", FileSystemNode::tree(&root));

        let names: Vec<_> = root.iter().map(|n| n.name()).collect();
        let expected = [
            "", "a", "e", "i", "f", "g", "h.lst", "b.txt", "c.dat", "d", "d.ext", "d.log", "j", "k",
        ];

        assert_eq!(names, expected);
    }

    #[test]
    fn test_part_one() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let root = parse_lines(&input).unwrap();
        let result = part_one(&root);

        assert_eq!(result, 95437)
    }

    #[test]
    fn test_part_two() {
        let input: Vec<_> = read_lines("inputs/example.txt").unwrap();
        let root = parse_lines(&input).unwrap();
        let result = part_two(&root).unwrap();

        assert_eq!(result, 24933642)
    }
}
