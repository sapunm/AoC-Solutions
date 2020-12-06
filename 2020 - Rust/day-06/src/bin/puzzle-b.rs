use day_06::input::{DataType, ResultType, DATA};
use itertools::Itertools;
use std::collections::HashSet;

fn run(data: &DataType) -> ResultType {
    data.iter()
        .map(|&group| {
            group.split_whitespace()
                .map(str::chars)
                .map(Iterator::collect::<HashSet<char>>)
                .fold1(|acc, answers| acc.intersection(&answers).cloned().collect())
                .unwrap()
                .len()
        })
        .sum()
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    let test = vec![
        "abc",
        "a b c",        
        "ab ac",
        "a a a a",
        "b",
    ];
    assert_eq!(run(&test), 6);
}
