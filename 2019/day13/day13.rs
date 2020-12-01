use std::fs::File;
use std::io::Read;
use std::collections::HashMap;

#[derive(Clone,Copy,Debug)]
enum Mode { Immediate, Address, Relative }

fn mode(x: i64) -> Mode {
    match x {
        0 => Mode::Address,
        1 => Mode::Immediate,
        2 => Mode::Relative,
        _ => panic!("illegal mode {}", x),
    }
}

fn parse_opcode(op: i64) -> (i64, Mode, Mode, Mode) {
    let mode1 = mode(op / 100 % 10);
    let mode2 = mode(op / 1000 % 10);
    let mode3 = mode(op / 10000 % 10);
    (op % 100, mode1, mode2, mode3)
}

struct Intcode {
    ip: i64,
    base: i64,
    memory: Vec<i64>,
}

enum StepResult {
    Input {
        address: i64,
    },
    Output {
        value: i64,
    },
    Ready,
    Done,
}

impl Intcode {
    fn arg(&self, mode: Mode, offset: i64) -> i64 {
        let immediate = self.memory[(self.ip + offset) as usize];
        match mode {
            Mode::Immediate => immediate,
            Mode::Address => self.memory[immediate as usize],
            Mode::Relative => self.memory[(self.base + immediate) as usize],
        }
    }

    fn oarg(&self, mode: Mode, offset: i64) -> i64 {
        let immediate = self.memory[(self.ip + offset) as usize];
        match mode {
            Mode::Immediate => immediate,
            Mode::Address => immediate,
            Mode::Relative => immediate + self.base,
        }
    }

    fn step(&mut self) -> StepResult {
        let (op, mode1, mode2, mode3) = parse_opcode(self.memory[self.ip as usize]);
        match op {
            // add
            1 => {
                let result = self.arg(mode1, 1) + self.arg(mode2, 2);
                let index = self.oarg(mode3, 3);
                self.memory[index as usize] = result;
                self.ip += 4;
                StepResult::Ready
            },
            // mul
            2 => {
                let result = self.arg(mode1, 1) * self.arg(mode2, 2);
                let index = self.oarg(mode3, 3);
                self.memory[index as usize] = result;
                self.ip += 4;
                StepResult::Ready
            },
            // input
            3 => {
                let address = self.oarg(mode1, 1);
                self.ip += 2;
                StepResult::Input { address }
            },
            // output
            4 => {
                let value = self.arg(mode1, 1);
                self.ip += 2;
                StepResult::Output { value }
            },
            // jtrue
            5 => {
                self.ip = if self.arg(mode1, 1) != 0 {
                    self.arg(mode2, 2)
                } else {
                    self.ip + 3
                };
                StepResult::Ready
            }
            // jfalse
            6 => {
                self.ip = if self.arg(mode1, 1) == 0 {
                    self.arg(mode2, 2)
                } else {
                    self.ip + 3
                };
                StepResult::Ready
            },
            // lt
            7 => {
                let result = if self.arg(mode1, 1) < self.arg(mode2, 2) { 1 } else { 0 };
                let index = self.oarg(mode3, 3);
                self.memory[index as usize] = result;
                self.ip += 4;
                StepResult::Ready
            },
            // eq
            8 => {
                let result = if self.arg(mode1, 1) == self.arg(mode2, 2) { 1 } else { 0 };
                let index = self.oarg(mode3, 3);
                self.memory[index as usize] = result;
                self.ip += 4;
                StepResult::Ready
            },
            // relbase
            9 => {
                self.base = self.base + self.arg(mode1, 1);
                self.ip += 2;
                StepResult::Ready
            },
            // halt
            99 => StepResult::Done,
            _ => panic!("invalid opcode {}", op),
        }
    }

    #[allow(unused)]
    fn run(&mut self, input: Vec<i64>) -> Vec<i64> {
        let mut output = vec![];
        let mut input: Vec<i64> = input.into_iter().rev().collect();
        loop {
            match self.step() {
                StepResult::Input { address } => {
                    let value = input.pop().expect("not enough input");
                    self.memory[address as usize] = value;
                },
                StepResult::Output { value } => {
                    output.push(value);
                },
                StepResult::Ready => continue,
                StepResult::Done => break,
            }
        }
        output
    }
}

struct Game {
    program: Intcode,
    board: HashMap<(i64, i64), i64>,
    buffered_outputs: Vec<i64>,
    ball_x: i64,
    paddle_x: i64,
    score: i64,
}

impl Game {
    #[allow(unused)]
    fn render(&self) {
        let (xs, ys) = self.board.keys().map(|k| *k).unzip::<i64, i64, Vec<i64>, Vec<i64>>();
        let xmin = *xs.iter().min().unwrap();
        let xmax = *xs.iter().max().unwrap();
        let ymin = *ys.iter().min().unwrap();
        let ymax = *ys.iter().max().unwrap();

        for y in ymin..ymax {
            for x in xmin..xmax {
                let tile = match self.board.get(&(x,y)).cloned().unwrap_or(0) {
                    0 => ' ',
                    1 => '#',
                    2 => 'X',
                    3 => {
                        assert_eq!(x, self.paddle_x);
                        '_'
                    },
                    4 => {
                        assert_eq!(x, self.ball_x);
                        'o'
                    },
                    _ => panic!("unknown tileid"),
                };
                print!("{}", tile);
            }
            print!("\n");
        }
    }

    fn process_output(&mut self, value: i64) {
        assert!(self.buffered_outputs.len() < 3);
        self.buffered_outputs.push(value);
        if self.buffered_outputs.len() == 3 {
            let x = self.buffered_outputs[0];
            let y = self.buffered_outputs[1];
            let p = self.buffered_outputs[2];
            if x == -1 {
                self.score = p;
            } else {
                if p == 3 {
                    self.paddle_x = x;
                } else if p == 4 {
                    self.ball_x = x;
                }
                self.board.insert((x,y),p);
            }
            self.buffered_outputs = vec![];
        }
    }

    fn play(&mut self) {
        loop {
            match self.program.step() {
                StepResult::Input { address } => {
                    let value = if self.ball_x < self.paddle_x {
                        -1
                    } else if self.ball_x > self.paddle_x {
                        1
                    } else {
                        0
                    };
                    self.program.memory[address as usize] = value;
                },
                StepResult::Output { value } => {
                    self.process_output(value);
                },
                StepResult::Ready => continue,
                StepResult::Done => break,
            }
        }
    }
}

fn read_input_file(filename: &str) -> Intcode {
    let mut file = File::open(filename).unwrap();
    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let memory = input.split_whitespace().map(|token| {
        i64::from_str_radix(token, 10).unwrap()
    }).chain(std::iter::repeat(0).take(100000)).collect();

    Intcode {
        ip: 0,
        base: 0,
        memory,
    }
}

fn main() {
    // let mut boost_program = read_input_file("boost.txt");
    // let output = boost_program.run(vec![1]);
    // println!("{:?}", output);

    let mut program = read_input_file("input.txt");
    program.memory[0] = 2;
    let mut game = Game {
        program,
        board: HashMap::new(),
        buffered_outputs: vec![],
        ball_x: 0,
        paddle_x: 0,
        score: 0,
    };
    game.play();
    println!("{}", game.score);
}
