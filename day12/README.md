# Day 12: Unison

That was a mistake.

I was hoping that I would be able to use [memo] for memoization. Unfortunately, Unison is *really really slow*, and memoization with Map wasn’t going to cut it.

I wrote my own memoization function based on `mutable.ByteArray.Raw`, which works a lot better, but is still really slow because Unison is slow, there’s no way around it.

Running Unison in CI proved to be a problem, because Unison operates on mutable “codebase” and there’s no straightforward way to compile a single Unison file. I ended up creating a temporary codebase in `build/`, which for some reason downloads base library every time, so CI check for this task alone takes ~40 seconds. You can’t run a single program multiple times in parallel, so all the parts are run sequentially, which takes about ~13 seconds of these 40.

The language itself is OK, I guess. Function search in UCM REPL is useful and effects are nice. 2 + 2 * 2 being 8 is a giant footgun though.

[memo]: https://share.unison-lang.org/@anovstrup/memo
