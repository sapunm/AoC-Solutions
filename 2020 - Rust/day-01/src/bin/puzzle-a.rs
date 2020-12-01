use aoc_utils::aoc::Input;
use day_01::parse::parse_input;

fn run(input: Input) -> usize {
    const SUM: usize = 2020;
    let mut seen_numbers = [false; SUM + 1];
    for number in parse_input(input) {
        if seen_numbers[SUM - number] {
            return number * (SUM - number);
        } else {
            seen_numbers[number] = true;
        }
    }
    0
}

fn main() {
    println!("{}", run(Input::stdin()));
}

#[test]
fn test_run() {
    let test_input = Input::bytes(b"1721\n\
    979\n\
    366\n\
    299\n\
    675\n\
    1456");
    assert_eq!(run(test_input), 514579);
}
