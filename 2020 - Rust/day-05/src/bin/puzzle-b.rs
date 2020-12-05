use day_05::input::{DataType, ResultType, DATA};

fn decode_pass(pass: &&str) -> ResultType {
    let row: ResultType = pass[0..7].chars()
        .fold((0, 127), |rows, part| {
            match part {
                'F' => (rows.0, (rows.0 + rows.1) / 2),
                'B' => ((rows.0 + rows.1) / 2 + 1, rows.1),
                _=> panic!("Unknown symbol: {}", part),
            }
        }).0;

    let col: ResultType = pass[7..=9].chars()
        .fold((0, 7), |cols, part| {
            match part {
                'L' => (cols.0, (cols.0 + cols.1) / 2),
                'R' => ((cols.0 + cols.1) / 2 + 1, cols.1),
                _=> panic!("Unknown symbol: {}", part),
            }
        }).0;
    
    row * 8 + col
}

fn run(data: &DataType) -> ResultType {
    let mut seats: Vec<usize> = data.iter()
        .map(decode_pass)
        .collect();

    seats.sort_unstable();

    for i in 1..seats.len()-1 {
        if seats[i] + 1 != seats[i + 1] {
            return seats[i] + 1;
        }
    }
    0
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    assert_eq!(run(&DATA), 579);
}
