use anyhow::anyhow;

fn crab_move(cups: &[u16]) -> Result<Vec<u16>, anyhow::Error> {
    let current = cups[0];
    let picked_up = &cups[1..4];
    let rest = &cups[4..];

    let mut target_value = current - 1;
    let dest_index = loop {
        let dest_index = rest
            .iter()
            .position(|x| *x == target_value && picked_up.iter().find(|p| *p == x).is_none());

        match dest_index {
            Some(d) => break d,
            None => {
                target_value = target_value
                    .checked_sub(1)
                    .or_else(|| rest.iter().max().map(|x| *x))
                    .ok_or_else(|| anyhow!("unable to find a target value"))?
            }
        }
    };

    Ok(rest[0..dest_index + 1]
        .iter()
        .chain(picked_up)
        .chain(&rest[dest_index + 1..])
        .chain(&[current])
        .map(|x| *x)
        .collect::<Vec<_>>())
}

fn cups_after_1(cups: &[u16]) -> String {
    cups.iter()
        .cycle()
        .skip_while(|x| **x != 1)
        .skip(1)
        .take_while(|x| **x != 1)
        .map(|x| format!("{}", x))
        .collect::<String>()
}

fn main() -> Result<(), anyhow::Error> {
    let shuffled_cups =
        (0..100).try_fold(vec![9, 5, 2, 4, 3, 8, 7, 1, 6], |cups, _| crab_move(&cups))?;
    println!("{:?}", cups_after_1(&shuffled_cups));

    // This runs pretty much instantly, but unfortunately doesn't scale for part 2..
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_passes_aoc_testcase() -> Result<(), anyhow::Error> {
        let shuffled_cups =
            (0..100).try_fold(vec![3, 8, 9, 1, 2, 5, 4, 6, 7], |cups, _| crab_move(&cups))?;
        assert_eq!(String::from("67384529"), cups_after_1(&shuffled_cups));
        Ok(())
    }
}
