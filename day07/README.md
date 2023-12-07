# Day 7: SWI Prolog

I’ve chosen Prolog for this day because it felt like a task from Prolog textbook and I was not mistaken.
That was really easy and pleasant. The main pain was trying to debug stuff when some computation just silently hangs.
I should definitely look into logic programming some more.

My implementation is somewhat optimized, by which I mean “is not the most dumb solution imaginable”
(you can find the most dumb in the first commit). It previously used `permutation/2` to evaluate hands,
but I replaced it with sorting and/or my own helper `combination/2`, resulting in x5 speedup.
The first version had an upside of being basically a copy of the task text, but I think this one still reads fine.

I expected I/O to be a problem, but I was able to just abuse CSV reader to do all the parsing.
