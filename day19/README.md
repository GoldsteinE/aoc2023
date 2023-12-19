# Day 19: Haskell

This task was fun! It was not immediately obvious how to solve it and it didn’t require visual thinking.

I chose Haskell because the solution can be written elegantly with monads.
Just check out `checkWorkflow` in [`Main.hs`], I think it’s really neat.
Of course, there was also a lot of boilerplate like inverting comparisons, overlapping ranges,
and parsing (which was much harder than it needed to be), but overall I think the code is pretty clean.

Thanks to [@s-and-witch] for showing me the `logict` library and proposing `WriterT` instead of manually passing accumulator!

[`Main.hs`]: ./code/app/Main.hs
[@s-and-witch]: https://github.com/s-and-witch
