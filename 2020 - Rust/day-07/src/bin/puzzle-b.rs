use day_07::input::{DataType, ResultType, DATA};
use std::collections::HashMap;
use itertools::Itertools;

fn count_bags(outer_bag: &str, bags_map: &HashMap<&str, HashMap<&str, usize>>) -> usize {
    bags_map[outer_bag].iter()
        .map(|(&bag, &count)| count * count_bags(bag, bags_map))
        .sum::<usize>() + 1
}

fn run(data: &DataType) -> ResultType {
    let bags = data.iter()
        .map(|&rule| {
            rule.split(" bags contain ")
                .collect_tuple::<(&str, &str)>()
                .unwrap()
        })
        .map(|(color, content)| (
            color,
            content.split(" bag")
                .map(|bag_desc| {
                    bag_desc.trim_start_matches(", ")
                        .trim_start_matches("s, ")
                        .trim_start_matches("s")
                })
                .filter(|&s| s != "" && s != "no other")
                .map(|s| {
                    let pos = s.find(" ").unwrap_or(0usize);
                    let num_bags = &s[..pos].parse::<usize>().unwrap();
                    (&s[pos + 1..], *num_bags)
                })
                .collect()
        ))
        .collect::<HashMap<_, _>>();
    
    count_bags("shiny gold", &bags) - 1
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    let test = vec![
        "light red bags contain 1 bright white bag, 2 muted yellow bags",
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags",
        "bright white bags contain 1 shiny gold bag",
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags",
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags",
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags",
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags",
        "faded blue bags contain no other bags",
        "dotted black bags contain no other bags",
    ];
    assert_eq!(run(&test), 32);
}
