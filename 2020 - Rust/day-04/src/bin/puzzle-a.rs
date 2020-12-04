use day_04::input::{DataType, DATA};

fn run(data: &DataType) -> usize {
    data.iter()
        .map(|passport| {
            passport.iter()
                .filter(|(field, _)| *field != "cid")
                .count()
        })
        .filter(|count| *count == 7usize)
        .count()
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    assert_eq!(run(&DATA), 204);
}
