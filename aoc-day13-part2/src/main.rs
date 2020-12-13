use std::{
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Copy, Clone, Debug, PartialEq)]
struct Bus {
    index: usize,
    step: isize,
}

trait MathTools {
    /// Inverse mod. Only works when p is prime.
    fn inv_mod(&self, p: isize) -> isize;
}

impl MathTools for isize {
    fn inv_mod(&self, p: isize) -> isize {
        (0..p - 2).fold(1, |o, _| (o * self) % p)
    }
}

fn parse(busses: &str) -> Vec<Bus> {
    busses
        .trim()
        .split(",")
        .map(|b| b.parse::<isize>().ok())
        .enumerate()
        .filter_map(|(index, step)| step.and_then(|step| Some(Bus { index, step })))
        .collect()
}

fn find_t(busses: &Vec<Bus>) -> isize {
    // Using Fermat's little theorem (https://en.wikipedia.org/wiki/Fermat%27s_little_theorem)
    // Inspired by u/altinus on https://www.reddit.com/r/rust/comments/kc5phc/advent_of_code_2020_day_13/
    // I don't really get it but it works

    let product = busses.iter().map(|b| b.step).product();
    busses
        .iter()
        .map(|b| {
            -(b.index as isize) * (product / b.step) * ((product / b.step) as isize).inv_mod(b.step)
        })
        .sum::<isize>()
        .rem_euclid(product)
}

fn main() -> Result<(), anyhow::Error> {
    let file = File::open("input.txt")?;
    let busses = BufReader::new(file).lines().nth(1).unwrap()?;
    let busses = parse(&busses);
    let t = find_t(&busses);
    println!("{}", t);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_busses() {
        assert_eq!(
            vec![
                Bus { index: 0, step: 17 },
                Bus { index: 2, step: 13 },
                Bus { index: 3, step: 19 }
            ],
            parse("17,x,13,19")
        );
    }

    #[test]
    fn it_passes_aoc_testcase() {
        let busses = parse("7,13,x,x,59,x,31,19");
        assert_eq!(1068781, find_t(&busses));
    }
}
