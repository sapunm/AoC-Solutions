pub mod parse {
    use std::io::BufRead;
    use aoc_utils::aoc::Input;

    pub fn parse_input(input: Input) -> impl Iterator<Item = usize> {
        input.reader
            .lines()
            .filter_map(Result::ok)
            .map(|line| line.parse::<usize>())
            .filter_map(Result::ok)
    }
}