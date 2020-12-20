use anyhow::anyhow;
use regex::Regex;
use std::{collections::HashMap, fs};
#[macro_use]
extern crate lazy_static;

#[derive(Debug, Clone, PartialEq)]
enum Rule {
    Letter(char),
    Subrules(Vec<u16>),
    Or(Vec<u16>, Vec<u16>),
}

impl Rule {
    fn parse(line: &str) -> Option<(u16, Self)> {
        let mut line = line.split(":");
        let id: u16 = line.next().and_then(|x| x.parse().ok())?;

        let rule = line.next()?;
        lazy_static! {
            static ref RE_CHAR: Regex = Regex::new(r"[a-z]").unwrap();
        }
        if RE_CHAR.is_match(rule) {
            let letter = RE_CHAR.find(rule).and_then(|c| c.as_str().chars().next())?;
            Some((id, Rule::Letter(letter)))
        } else if rule.contains("|") {
            let mut subrules = rule.split("|");
            let sr1 = subrules
                .next()
                .and_then(|sr| sr.split_whitespace().map(|n| n.parse().ok()).collect())?;
            let sr2 = subrules
                .next()
                .and_then(|sr| sr.split_whitespace().map(|n| n.parse().ok()).collect())?;
            Some((id, Rule::Or(sr1, sr2)))
        } else {
            Some((
                id,
                Rule::Subrules(
                    rule.split_ascii_whitespace()
                        .filter_map(|n| n.parse().ok())
                        .collect(),
                ),
            ))
        }
    }

    /// Applies a rule to a message, "consuming" it's part.
    /// Returns Ok(the_rest_of_the_message). Or Errors if it cannot be applied
    fn apply(&self, message: &str, rules: &HashMap<u16, Rule>) -> Result<String, anyhow::Error> {
        match &self {
            Rule::Letter(c) => match message.chars().next() {
                Some(x) if x == *c => Ok(String::from(&message[1..])),
                _ => Err(anyhow!("Non-matching string")),
            },
            Rule::Subrules(subrules) => {
                subrules
                    .into_iter()
                    .try_fold(String::from(message), |acc, el| {
                        let rule = rules.get(el).ok_or(anyhow!("rule did not exist"))?;
                        rule.apply(&acc, &rules)
                    })
            }
            Rule::Or(subrules1, subrules2) => {
                match subrules1
                    .into_iter()
                    .try_fold(String::from(message), |acc, el| {
                        let rule = rules.get(el).ok_or(anyhow!("rule did not exist"))?;
                        rule.apply(&acc, &rules)
                    }) {
                    Ok(v) => Ok(v),
                    Err(_) => subrules2
                        .into_iter()
                        .try_fold(String::from(message), |acc, el| {
                            let rule = rules.get(el).ok_or(anyhow!("rule did not exist"))?;
                            rule.apply(&acc, &rules)
                        }),
                }
            }
        }
    }

    fn is_valid(&self, message: &str, rules: &HashMap<u16, Rule>) -> bool {
        match self.apply(message, rules) {
            Ok(v) => v.len() == 0,
            Err(_) => false,
        }
    }
}

fn matches(
    messages: &[String],
    rule: u16,
    rules: &HashMap<u16, Rule>,
) -> Result<Vec<String>, anyhow::Error> {
    let rule = rules.get(&rule).ok_or(anyhow!("rule did not exist"))?;

    let matching_messages = messages
        .into_iter()
        .filter(|m| rule.is_valid(&m, &rules))
        .map(String::from)
        .collect();

    Ok(matching_messages)
}

fn main() -> Result<(), anyhow::Error> {
    let input = fs::read_to_string("input.txt")?;
    let mut input = input.split("\n\n");

    let rules = input
        .next()
        .unwrap_or_default()
        .trim()
        .lines()
        .filter_map(Rule::parse)
        .collect();

    let messages: Vec<_> = input
        .next()
        .unwrap_or_default()
        .trim()
        .lines()
        .map(String::from)
        .collect();

    // Part 1
    let matching_zero = matches(&messages, 0, &rules)?;
    println!(
        "Number of messages matching rule 0: {}",
        matching_zero.len()
    );

    // Didn't do Part 2, not sure if I get it..

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_passes_aoc_testcase() -> Result<(), anyhow::Error> {
        let rules = [
            (0, Rule::Subrules(vec![4, 1, 5])),
            (1, Rule::Or(vec![2, 3], vec![3, 2])),
            (2, Rule::Or(vec![4, 4], vec![5, 5])),
            (3, Rule::Or(vec![4, 5], vec![5, 4])),
            (4, Rule::Letter('a')),
            (5, Rule::Letter('b')),
        ]
        .iter()
        .cloned()
        .collect();

        let messages = vec![
            String::from("ababbb"),
            String::from("bababa"),
            String::from("abbbab"),
            String::from("aaabbb"),
            String::from("aaaabbb"),
        ];

        let matches = matches(&messages, 0, &rules)?;

        assert_eq!(2, matches.len());
        Ok(())
    }
}
