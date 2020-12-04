use day_04::input::{DataType, DATA};
use rayon::prelude::*;
use regex::Regex;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref HCL_REGEX: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    static ref ECL_REGEX: Regex = Regex::new(r"^(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)$").unwrap();
    static ref PID_REGEX: Regex = Regex::new(r"^\d{9}$").unwrap();
}

fn field_valid(passport_field: &(&str, &str)) -> bool {
    let (field, val) = passport_field;

    match *field {
        "byr" => match val.parse::<i32>() {
            Ok(x) => x >= 1920 && x <= 2002,
            Err(_) => false,
        },
        "iyr" => match val.parse::<i32>() {
            Ok(x) => x >= 2010 && x <= 2020,
            Err(_) => false,
        },
        "eyr" => match val.parse::<i32>() {
            Ok(x) => x >= 2020 && x <= 2030,
            Err(_) => false,
        },
        "hgt" => {                    
            let len = val.len();                    
            match &val[len-2..] {
                "cm" => match val[..len-2].parse::<i32>() {
                    Err(_) => false,
                    Ok(x) => x>=150 && x <= 193                           
                },
                "in" => match val[..len-2].parse::<i32>() {
                    Err(_) => false,
                    Ok(x) => x>=59 && x <= 76                           
                },
                _ => false,
            }
        },
        "hcl" => HCL_REGEX.is_match(val),
        "ecl" => ECL_REGEX.is_match(val),
        "pid" => PID_REGEX.is_match(val),
        _ =>false
    }
}

fn passport_valid(passport: &Vec<(&str, &str)>) -> bool {
    let num_valid_fields = passport.iter()
        .filter(|field| field_valid(*field))
        .count();
    num_valid_fields == 7
}

fn run(data: &DataType) -> usize {
    data.par_iter()
        .filter(|passport| passport_valid(*passport))        
        .count()
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    assert_eq!(run(&DATA), 179);
}
