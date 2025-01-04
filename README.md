Solving sudo in Rust with constraint propagation
==

This code was written as part of a blogpost that should be available on
[kaveland.no](https://kaveland.no/posts/2025-01-04-mutual-recursion-for-fun-and-profit/),
about constraint propagation, advent of code and sudoko. All of this code was written as 
throwaway-code, although maybe some of it will move on to [advent-of-code-rs](https://github.com/kaaveland/advent-of-code-rs).

Installation
--

You need a rust toolchain, I usually recommend [rustup](https://www.rust-lang.org/tools/install).

Testing
--

`cargo test`

Running
--

To solve a sudoko:

`cargo run < your_sudo.txt`

To run on some advent of code input:

`cargo run aoc < input.txt`