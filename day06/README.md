# Day 6: GNU Assembler (w/o libc)

This solution is 100%, absolutely naive and it executes (including compilation step) in ~50ms on my laptop. I feel like numbers in part 2 should have been bigger.
The code itself is heavily commented to improve readability.

I used assembler for this one because it resonated with the idea of just bruteforcing it *fast*. Unlike [2020] and [2021] I did not use any particular calling convention,
instead opting for individualizing cconv for each function. This saved me quite a bit of register shuffling. The solution doesn’t even use the stack except for `call` and `ret` (it does use a couple of pre-allocated static buffers though).

As it doesn’t use libc, it will only run on x86_64 Linux.

[2020]: https://github.com/GoldsteinE/AdventOfAsm2020
[2021]: https://github.com/GoldsteinE/aoc2021/tree/master/day1
