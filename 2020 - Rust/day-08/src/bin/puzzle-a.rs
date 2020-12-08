use day_08::input::*;

fn boot(data: &DataType) -> (ResultType, ResultType) {
    let mut visited = vec![false; data.len()];
    let mut ip: ResultType = 0;
    let mut acc: ResultType = 0;
    loop {
        visited[ip as usize] = true;
        let (op_code, param) = &data[ip as usize];
        match op_code {
            OpCode::Acc => { acc += param; ip += 1 },
            OpCode::Jmp => ip += *param,
            OpCode::Nop => ip += 1,
        }
        if ip as usize == data.len() || visited[ip as usize] {
            return (ip, acc);
        }
    }
    unreachable!()
}

fn run(data: &DataType) -> ResultType {
    boot(&data).1
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    assert_eq!(run(&DATA), 1744);
}
