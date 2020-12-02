pub mod parse {
    use std::io::BufRead;
    use aoc_utils::aoc::Input;

    pub fn parse_input(input: Input) -> impl Iterator<Item = (usize, usize, char, String)> {
        input.reader
            .lines()
            .filter_map(Result::ok)
            .map(|line| line.split(' ')
                                   .flat_map(|s| s.trim_end_matches(':').split('-'))
                                   .map(String::from)
                                   .collect::<Vec<_>>())
            .map(|v| (
                v[0].parse::<usize>().unwrap(),
                v[1].parse::<usize>().unwrap(),
                v[2].chars().next().unwrap(),
                v[3].to_string()
            ))
    }
}