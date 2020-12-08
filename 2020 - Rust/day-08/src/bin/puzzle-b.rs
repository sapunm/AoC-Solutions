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

fn flip_instruction(data: &mut DataType, idx: usize) {
    match data[idx].0 {
        OpCode::Jmp => data[idx].0 = OpCode::Nop,
        OpCode::Nop => data[idx].0 = OpCode::Jmp,
        _ => (),
    }
}

fn run(data: &DataType) -> ResultType {
    let mut data_mut = data.clone();
    let mut i: usize = 0;
    loop {
        flip_instruction(&mut data_mut, i);
        let (ip, acc) = boot(&data_mut);
        if ip as usize == data.len() {
            return acc;
        }
        flip_instruction(&mut data_mut, i);
        i += 1
    }
    unreachable!()
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    assert_eq!(run(&DATA), 1174);
}
