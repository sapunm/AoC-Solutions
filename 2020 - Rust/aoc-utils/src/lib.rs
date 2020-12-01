pub mod aoc {
    #[allow(unused_imports)]
    use std::fs;
    use std::io::{self, BufReader, BufRead};

    pub struct Input {
        pub reader: Box<dyn BufRead>,
    }

    impl Input {
        pub fn stdin() -> Input {
            Input {
                reader: Box::new(BufReader::new(io::stdin()))
            }
        }

        pub fn file(path: &str) -> Input {
            Input {
                reader: Box::new(BufReader::new(fs::File::open(path).unwrap()))
            }
        }

        pub fn bytes(bytes: &'static [u8]) -> Input {
            Input {
                reader: Box::new(BufReader::new(bytes))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
