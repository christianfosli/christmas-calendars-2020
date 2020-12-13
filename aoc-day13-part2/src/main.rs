use rayon::prelude::*;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn parse(busses: &str) -> Vec<Option<usize>> {
    busses.split(",").map(|b| b.parse::<usize>().ok()).collect()
}

fn next_dep(timestamp: usize, buss: usize) -> usize {
    buss * ((timestamp as f64 / buss as f64).ceil() as usize)
}

fn is_sequential(busses: &Vec<Option<usize>>, timestamp: usize) -> bool {
    busses
        .into_iter()
        .map(|x| match x {
            Some(x) => Some(next_dep(timestamp, *x)),
            None => None,
        })
        .enumerate()
        .all(|(i, el)| match el {
            Some(t) => t - timestamp == i,
            None => true,
        })
}

fn find_first_sequential_ts(busses: &Vec<Option<usize>>) -> Option<usize> {
    let first_bus = busses[0].unwrap();
    (0..).step_by(first_bus).find(|ts| {
        if ts % 1_000_000 == 0 {
            println!("-");
        }
        is_sequential(busses, *ts)
    })
}

fn par_find_first_sequential_ts(busses: &Vec<Option<usize>>) -> Option<usize> {
    let first_bus = busses[0].unwrap();
    (0..std::u64::MAX as usize)
        .into_par_iter()
        .step_by(first_bus)
        .find_any(|ts| is_sequential(busses, *ts))
}

fn main() -> Result<(), anyhow::Error> {
    let file = File::open("input.txt")?;
    let busses = BufReader::new(file).lines().skip(1).next().unwrap()?;
    let busses = parse(&busses);
    let the_one = par_find_first_sequential_ts(&busses);
    println!("{:?}", the_one);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_busses() {
        assert_eq!(
            vec![Some(17), None, Some(13), Some(19)],
            parse("17,x,13,19")
        );
    }

    #[test]
    fn it_passes_aoc_testcase() {
        let busses = parse("7,13,x,x,59,x,31,19");
        assert_eq!(Some(1068781), find_first_sequential_ts(&busses));
    }
}
