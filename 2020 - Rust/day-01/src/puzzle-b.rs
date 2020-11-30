fn run() -> String {
    String::from("Hello, world!")
}

fn main() {
    println!("{}", run());
}

#[test]
fn test_add() {
    assert_eq!(run(), "Hello, world!");
}
