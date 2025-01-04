use std::env::args;
use regex::Regex;
use std::fmt::Debug;
use std::io::{stdin, Read};
use Instruction::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Addr,
    Addi,
    Mulr,
    Muli,
    Banr,
    Bani,
    Borr,
    Bori,
    Setr,
    Seti,
    Gtir,
    Gtri,
    Gtrr,
    Eqir,
    Eqri,
    Eqrr,
}

fn evaluate(
    instruction: Instruction,
    arg_a: usize,
    arg_b: usize,
    arg_c: usize,
    mut registers: [usize; 4],
) -> [usize; 4] {
    use Instruction::*;
    registers[arg_c] = match instruction {
        Addr => registers[arg_a] + registers[arg_b],
        Addi => registers[arg_a] + arg_b,
        Mulr => registers[arg_a] * registers[arg_b],
        Muli => registers[arg_a] * arg_b,
        Banr => registers[arg_a] & registers[arg_b],
        Bani => registers[arg_a] & arg_b,
        Borr => registers[arg_a] | registers[arg_b],
        Bori => registers[arg_a] | arg_b,
        Setr => registers[arg_a],
        Seti => arg_a,
        Gtir => usize::from(arg_a > registers[arg_b]),
        Gtri => usize::from(registers[arg_a] > arg_b),
        Gtrr => usize::from(registers[arg_a] > registers[arg_b]),
        Eqir => usize::from(arg_a == registers[arg_b]),
        Eqri => usize::from(registers[arg_a] == arg_b),
        Eqrr => usize::from(registers[arg_a] == registers[arg_b]),
    };
    registers
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
struct Example {
    before: [usize; 4],
    program: [usize; 4],
    after: [usize; 4],
}

fn parse(s: &str) -> impl Iterator<Item = Example> + '_ {
    let number = Regex::new(r"\d+").unwrap();
    s.split("\n\n").map(move |ex| {
        let mut example = Example::default();
        for (index, matched) in number.find_iter(ex).enumerate() {
            let n: usize = matched.as_str().parse().unwrap();
            let rem = index % 4;
            match index / 4 {
                0 => example.before[rem] = n,
                1 => example.program[rem] = n,
                2 => example.after[rem] = n,
                _ => panic!("Too many numbers in block: {index}, {ex}"),
            }
        }
        example
    })
}

const ALL: [Instruction; 16] = [
    Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr,
];

fn compatible_instructions(example: &Example) -> usize {
    ALL.into_iter()
        .filter(|instruction| {
            let [_, arg_a, arg_b, arg_c] = example.program;
            let out = evaluate(*instruction, arg_a, arg_b, arg_c, example.before);
            out == example.after
        })
        .count()
}

fn part_1(s: &str) -> usize {
    let (examples, _) = s.split_once("\n\n\n").unwrap();
    parse(examples)
        .filter(|example| compatible_instructions(example) >= 3)
        .count()
}

type Options = [u16; 16];

fn initial_options() -> Options {
    [0xffff; 16]
}

fn identify_opcodes(examples: &[Example]) -> [usize; 16] {
    let mut options = initial_options();
    for example in examples.into_iter() {
        let [opcode, arg_a, arg_b, arg_c] = example.program;
        for (place, instruction) in ALL.into_iter().enumerate() {
            let out = evaluate(instruction, arg_a, arg_b, arg_c, example.before);
            if out != example.after {
                options.eliminate(opcode, place);
            }
        }
        // Check if we're done
        if options.iter().all(|possible| possible.count_ones() == 1) {
            break;
        }
    }
    let mut choices = [0; 16];
    for place in 0..options.len() {
        if let Some(choice) = options.choice_made(place) {
            choices[place] = choice;
        }
    }
    choices
}

fn part_2(s: &str) -> usize {
    let (examples, program) = s.split_once("\n\n\n").unwrap();
    let examples: Vec<_> = parse(examples).collect();
    let mapping = identify_opcodes(&examples);
    let mut registers = [0; 4];
    let mut command = [0; 4];
    let number = Regex::new(r"\d+").unwrap();
    for line in program.lines() {
        for (idx, n) in number.find_iter(line).enumerate() {
            command[idx] = n.as_str().parse().unwrap();
        }
        let [opcode, arg_a, arg_b, arg_c] = command;
        registers = evaluate(ALL[mapping[opcode]], arg_a, arg_b, arg_c, registers);
    }
    registers[0]
}

trait Possibilities<K: Copy, V: Copy> {
    fn set(&mut self, place: K, value: V);
    fn remove(&mut self, place: K, value: V) -> bool;
    fn choice_made(&self, place: K) -> Option<V>;
    fn neighbours(&self, place: K) -> impl Iterator<Item = K>;

    fn eliminate(&mut self, place: K, value: V) {
        if self.remove(place, value) {
            if let Some(choice) = self.choice_made(place) {
                self.choose(place, choice);
            }
        }
    }
    fn choose(&mut self, place: K, value: V) {
        self.set(place, value);
        for target in self.neighbours(place).collect::<Vec<_>>() {
            self.eliminate(target, value);
        }
    }
}

impl<const N: usize> Possibilities<usize, usize> for [u16; N] {
    fn set(&mut self, place: usize, value: usize) {
        self[place] = 1 << value;
    }

    fn remove(&mut self, place: usize, value: usize) -> bool {
        let bit = 1 << value;
        let set = self[place] & bit;
        self[place] &= !bit;
        set > 0
    }

    fn choice_made(&self, place: usize) -> Option<usize> {
        if self[place].count_ones() == 1 {
            Some(self[place].trailing_zeros() as usize)
        } else {
            None
        }
    }

    fn neighbours(&self, place: usize) -> impl Iterator<Item = usize> {
        (0..N).filter(move |n| *n != place)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct SudokuOptions {
    options: [u16; 81],
}

impl Default for SudokuOptions {
    fn default() -> Self {
        // avoiding the zeroth bit will make it easier for us to make output later
        let everything_possible = ((1 << 10) - 1) ^ 1;
        SudokuOptions {
            options: [everything_possible; 81],
        }
    }
}

impl Possibilities<(usize, usize), u8> for SudokuOptions {
    fn set(&mut self, place: (usize, usize), value: u8) {
        let ix = place.0 + place.1 * 9;
        self.options[ix] = 1 << value;
    }

    fn remove(&mut self, place: (usize, usize), value: u8) -> bool {
        let ix = place.0 + place.1 * 9;
        let bit = 1 << value;
        let set = self.options[ix] & bit;
        self.options[ix] &= !bit;
        set > 0
    }

    fn choice_made(&self, place: (usize, usize)) -> Option<u8> {
        let ix = place.0 + place.1 * 9;
        if self.options[ix].count_ones() == 1 {
            Some(self.options[ix].trailing_zeros() as u8)
        } else {
            None
        }
    }

    fn neighbours(&self, place: (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
        let (x, y) = place;
        let row = (0..9).map(move |x| (x, y));
        let col = (0..9).map(move |y| (x, y));
        let (xstart, ystart) = (3 * (x / 3), 3 * (y / 3));
        let square = (0..9).map(move |s| (xstart + s / 3, ystart + s % 3));
        row.chain(col)
            .chain(square)
            .filter(move |neighbour| *neighbour != place)
    }
}

fn sudoku(puzzle: &str) -> SudokuOptions {
    let mut board = SudokuOptions::default();
    let known = puzzle.lines().enumerate().flat_map(|(y, line)| {
        line.as_bytes()
            .iter()
            .enumerate()
            .filter_map(move |(x, ch)| {
                if *ch != b'.' {
                    Some(((x, y), *ch - b'0'))
                } else {
                    None
                }
            })
    });
    for (place, value) in known {
        board.choose(place, value);
    }
    board
}


impl SudokuOptions {
    fn try_to_string(&self) -> Result<String, String> {
        let mut out = String::new();
        for y in 0..9 {
            for x in 0..9 {
                if let Some(choice) = self.choice_made((x, y)) {
                    out.push((choice + b'0') as char);
                } else {
                    return Err(format!("{x}, {y} unresolved"));
                }
            }
            out.push('\n');
        }
        Ok(out)
    }
}

fn main() -> Result<(), String> {
    let mut input = String::new();
    stdin().lock().read_to_string(&mut input)
        .map_err(|err| format!("{err}"))?;
    let do_aoc = args().any(|arg| arg.as_str() == "aoc");

    if do_aoc {
        let p1 = part_1(input.as_str());
        let p2 = part_2(input.as_str());
        println!("Part 1: {p1}, Part 2: {p2}");
    } else {
        let board = sudoku(input.as_str());
        let out = board.try_to_string()?;
        println!("{out}");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_opcode() {
        let example = Example {
            before: [3, 2, 1, 1],
            program: [9, 2, 1, 2],
            after: [3, 2, 2, 1]
        };
        let expect = [Mulr, Addi, Seti];
        for ins in ALL {
            assert_eq!(
                evaluate(ins, 2, 1, 2, example.before) == example.after,
                expect.contains(&ins)
            );
        }

    }

    const EASY_PUZZLE: &str = "53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79";
    const EASY_SOLUTION: &str = "534678912
672195348
198342567
859761423
426853791
713924856
961537284
287419635
345286179";

    #[test]
    fn on_easy_sudoku() {
        let sol = sudoku(EASY_PUZZLE).try_to_string().unwrap();
        assert_eq!(sol.as_str().trim(), EASY_SOLUTION);
    }

}
