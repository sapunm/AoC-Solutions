use aoc_utils::aoc::Input;
use day_01::parse::parse_input;

fn run(input: Input) -> usize {
    const SUM: usize = 2020;
    let numbers: Vec<usize> = parse_input(input).collect();
    
    for (i, num1) in numbers.iter().enumerate() {
        let mut seen_numbers = [false; SUM + 1];
        let sub_sum: usize = SUM - num1;
        for (j, num2) in numbers.iter().enumerate() {
            if i != j && sub_sum > *num2 {
                let num3: usize = sub_sum - num2;
                if seen_numbers[num3] {
                    return num1 * num2 * num3;
                } else {
                    seen_numbers[*num2] = true;
                }
            }
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
    assert_eq!(run(test_input), 241861950);
}
