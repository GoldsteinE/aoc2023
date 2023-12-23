# Day 23: Racket

This one is actually NP-hard in general case.
It’s also NP-hard in this specific case, but the input could be minimized so bruteforce runs in a reasonable-ish amount of time.

I solved it in Racket, because the Advent is nearing its end and I wanted to use LISP somewhere.
The solution is basically a bunch of folds, so I figured LISP would fit quite nicely.
In retrospect, this might have been a mistake, since Racket is quite slow, so my solution runs in ~30 seconds for part 2.

Racket is pretty nice. I really missed something like `for/filter-list` though (rough equivalent of `.filter().collect()` in Rust).
It can be quite easily emulated with `for/fold`, but it’s much less pleasant.
