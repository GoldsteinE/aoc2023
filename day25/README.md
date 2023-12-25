# Day 25: Rust

Rust is my current preferred language. I use it at work and for most of my pet projects.
It seemed fitting to save it for the last day.

The task itself was interesting and I almost certainly used the “wrong” solution.
My code uses Karger’s probabilistic algorithm, retrying it with a different seed until it finds the answer.
One other possible solution would be to use Stoer–Wagner algorithm, which is deterministic, but has somewhat higher complexity.
I still think it would probably be faster on average.
