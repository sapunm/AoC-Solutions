use day_03::input::{DataType, DATA};

fn get_data_dim(data: &DataType) -> (usize, usize) {
    ( data.len(),
      match data.len() {
          0 => 0,
          _ => data[0].len()
    })
}

fn run(data: &DataType) -> usize {
    let dim = get_data_dim(data);
    let trajectory = (1usize, 3usize);
    let mut pos = (0usize, 0usize);
    let mut num_trees = 0usize;
    while pos.0 < dim.0 {
        num_trees += data[pos.0][pos.1] as usize;
        pos = (pos.0 + trajectory.0, (pos.1 + trajectory.1) % dim.1)
    }
    num_trees
}

fn main() {
    println!("{}", run(&DATA));
}

#[test]
fn test_run() {
    let test = vec![
        vec![0,0,1,1,0,0,0,0,0,0,0],
        vec![1,0,0,0,1,0,0,0,1,0,0],
        vec![0,1,0,0,0,0,1,0,0,1,0],
        vec![0,0,1,0,1,0,0,0,1,0,1],
        vec![0,1,0,0,0,1,1,0,0,1,0],
        vec![0,0,1,0,1,1,0,0,0,0,0],
        vec![0,1,0,1,0,1,0,0,0,0,1],
        vec![0,1,0,0,0,0,0,0,0,0,1],
        vec![1,0,1,1,0,0,0,1,0,0,0],
        vec![1,0,0,0,1,1,0,0,0,0,1],
        vec![0,1,0,0,1,0,0,0,1,0,1],
    ];
    assert_eq!(run(&test), 7);
}
