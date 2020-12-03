use rayon::prelude::*;
use day_03::input::{DataType, DATA};

type Dim = (usize, usize);
type Pos = Dim;
type Trajectory = Dim;

fn get_data_dim(data: &DataType) -> Dim {
    ( data.len(),
      match data.len() {
          0 => 0,
          _ => data[0].len()
    })
}

fn count_trees(data: &DataType) -> impl '_ + Fn(&Trajectory) -> usize {
    move |trajectory| {
        let dim: Dim = get_data_dim(data);
        let mut pos: Pos = (0, 0);
        let mut num_trees: usize = 0;
        while pos.0 < dim.0 {
            num_trees += data[pos.0][pos.1] as usize;
            pos = (pos.0 + trajectory.0, (pos.1 + trajectory.1) % dim.1)
        }
        num_trees
    }
}

fn run(data: &DataType) -> usize {
    let trajectories: Vec<Trajectory> = vec![
        (1, 1),
        (1, 3),
        (1, 5),
        (1, 7),
        (2, 1),
    ];
    trajectories.par_iter()
        .map(count_trees(data))
        .product()
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
    assert_eq!(run(&test), 336);
}
