use day_06::input::{DataType, ResultType, DATA};
use itertools::Itertools;

fn run(data: &DataType) -> ResultType {
    data.iter()
        .map(|&group| {
            group.chars()
            .filter(|c| !c.is_whitespace())
            .unique()
            .count()
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
    assert_eq!(run(&test), 11);
}
