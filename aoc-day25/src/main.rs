// Used to generate pub key
fn transform_sub_num(sub: usize, loop_size: usize) -> usize {
    (0..loop_size).fold(1, |acc, _| (acc * sub) % 20201227)
}

fn find_loop_size(sub: usize, pub_key: usize) -> Option<usize> {
    (1..).find(|loop_size| transform_sub_num(sub, *loop_size) == pub_key)
}

fn find_encryption_key(my_loop_size: usize, other_pub_key: usize) -> usize {
    transform_sub_num(other_pub_key, my_loop_size)
}

fn main() {
    let card_pub_key = 10943862;
    let door_pub_key = 12721030;

    let encryption_key = find_loop_size(7, card_pub_key)
        .map(|loop_size| find_encryption_key(loop_size, door_pub_key));

    println!("{:?}", encryption_key);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_can_find_loop_size() {
        let card_pub_key = 5764801;
        assert_eq!(Some(8), find_loop_size(7, card_pub_key));

        let door_pub_key = 17807724;
        assert_eq!(Some(11), find_loop_size(7, door_pub_key));
    }

    #[test]
    fn it_can_find_encryption_key() {
        assert_eq!(14897079, find_encryption_key(8, 17807724));
    }
}
