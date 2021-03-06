use aoc_utils::aoc::Input;
use day_02::parse::parse_input;

fn password_valid(data: (usize, usize, char, String)) -> bool {
    let (min, max, letter, password) = data;
    let chars: Vec<char> = password.chars().collect();    
    (chars[min - 1] == letter) ^ (chars[max-1] == letter)
}

fn run(input: Input) -> usize {
    parse_input(input).map(password_valid).filter(|x| *x).count()
}

fn main() {
    println!("{}", run(Input::stdin()));
}

#[test]
fn test_run() {
    let test_input = Input::bytes(b"1-3 a: abcde\n\
    1-3 b: cdefg\n\
    2-9 c: ccccccccc");
    assert_eq!(run(test_input), 1);
}
