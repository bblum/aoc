use std::fs::File;
use std::io::Read;

fn anyn(noob: &str, n: usize) -> bool {
    "abcdefghijklmnopqrstuvwxyz".chars().any(|x| noob.chars().filter(|c| *c == x).count() == n)
}

fn countn(input: &String, n: usize) -> usize {
    input.lines().filter(|line| anyn(line, n)).count()
}

fn main() {
    let mut file = File::open("input.txt").unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();
    // part 1
    println!("{}", countn(&input, 2) * countn(&input, 3));
    // part 2
    for (i, x) in input.lines().enumerate() {
        for y in input.lines().skip(i) {
            let z: String = x.chars().zip(y.chars()).filter_map(|(x,y)| if x == y { Some(x) } else { None }).collect();
            if z.len() == x.len() - 1 {
                println!("{}", z);
            }
        }
    }
}
