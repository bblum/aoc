const INPUT: [usize; 9] = [5,8,3,9,7,6,2,4,1];

fn find_dest(current: usize, max: usize, taken: &[usize]) -> usize {
    let dest = if current == 1 { max } else { current - 1 };
    if taken.iter().any(|&cup| cup == dest) {
        find_dest(dest, max, taken)
    } else {
        dest
    }
}

fn run(max: usize, steps: usize) -> Vec<usize> {
    let mut next_cups = vec![0; max+1];
    let mut current = INPUT[0];

    let mut sequence = INPUT.iter().cloned().chain(10..=max).peekable();
    while let Some(i) = sequence.next() {
        next_cups[i] = *sequence.peek().unwrap_or(&current);
    }

    for _ in 0..steps {
        let cup1 = next_cups[current];
        let cup2 = next_cups[cup1];
        let cup3 = next_cups[cup2];
        next_cups[current] = next_cups[cup3];

        let dest = find_dest(current, max, &[cup1, cup2, cup3]);
        next_cups[cup3] = next_cups[dest];
        next_cups[dest] = cup1;

        current = next_cups[current];
    }

    next_cups
}

fn main() {
    let part1 = run(9, 100);
    let mut i = 1;
    for i in std::iter::from_fn(|| { i = part1[i]; if i == 1 { None } else { Some(i) } }) {
        print!("{}", i);
    }
    println!();

    let part2 = run(1_000_000, 10_000_000);
    println!("{}", part2[1] * part2[part2[1]]);
}
