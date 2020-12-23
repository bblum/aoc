// const INPUT: [usize; 9] = [3,8,9,1,2,5,4,6,7];
const INPUT: [usize; 9] = [5,8,3,9,7,6,2,4,1];

const SIZE: usize = 1_000_000;

#[derive(Default, Clone, Copy)]
struct Nobe {
    prev: usize,
    next: usize,
}

fn crab(cups: &mut [Nobe], current: &mut usize) {
    let cup1 = cups[*current].next;
    let cup2 = cups[cup1].next;
    let cup3 = cups[cup2].next;

    // remove from da cicrle
    let next = cups[cup3].next;
    cups[*current].next = next;
    cups[next].prev = *current;

    let mut dest = if *current == 1 { SIZE } else { *current-1 };
    if dest == cup1 || dest == cup2 || dest == cup3 {
        dest = if dest == 1 { SIZE } else { dest-1 };
    }
    if dest == cup1 || dest == cup2 || dest == cup3 {
        dest = if dest == 1 { SIZE } else { dest-1 };
    }
    if dest == cup1 || dest == cup2 || dest == cup3 {
        dest = if dest == 1 { SIZE } else { dest-1 };
    }

    // println!("current {}; picked up [{},{},{}], dest -> {}", *current, cup1, cup2, cup3, dest);

    let newnext = cups[dest].next;
    cups[dest].next = cup1;
    cups[cup1].prev = dest;
    cups[cup3].next = newnext;
    cups[newnext].prev = cup3;

    // have to check current's link again because current might have been the destination
    *current = cups[*current].next;
    // println!("update current -> {}", *current);
}

fn print(cups: &[Nobe], mut current: usize, n: usize) {
    for _ in 0..n {
        print!("{} ", current);
        assert_eq!(current, cups[cups[current].next].prev);
        current = cups[current].next;
    }
    println!();
}

fn main() {
    let mut index: Vec<Nobe> = vec![Default::default(); SIZE+1];
    let mut current = INPUT[0];
    let mut prev = 999999999;
    assert!(prev > SIZE);

    for i in 1..=SIZE {
        let n = if i <= 9 {
            INPUT[i-1]
        } else {
            i
        };
        if prev != 999999999 {
            index[n].prev = prev;
            index[prev].next = n;
            //println!("{} <-> {}", prev, n);
        }
        prev = n;
    }
    println!("finalizing, {} <-> {}", prev, current);
    index[prev].next = current;
    index[current].prev = prev;

    // for i in 1..=SIZE {
    //     println!("index[{}]: {} {}", i, index[i].prev, index[i].next);
    //     assert_ne!(index[i].prev, 0);
    //     assert_ne!(index[i].next, 0);
    // }

    for i in 0..10_000_000 {
        if i % 100_000 == 0 {
            println!("progress.. {}%", i / 100_000);
        }
        crab(&mut index, &mut current);
    }
    // print(&index, current, SIZE);
    let c1 = index[1].next;
    let c2 = index[c1].next;
    println!("{} {} -> {}", c1, c2, c1*c2);
}
