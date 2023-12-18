# Day 18: Factor

The task itself was pretty much a repetition of [day 10].

I chose Factor because this task didn’t require any complex data structures.
I didn’t particularly like the experience: *a lot* of my code is stack shuffling and I'm not sure how to reduce the amount of it.
Readability of the main functions is basically zero, because you need to keep track of 8-ish stack items.

HOFs were another pain point: stack effect checker is not smart enough to properly deal with them, so you can’t do e.g.

```factor
! pass `f` to `hof` if condition is true, else pass `g`
condition [ f ] [ g ] ? hof
```

and you need to instead do

```factor
condition [ [ f ] hof ] [ [ g ] hof ] if
```

I may try this later in Kitten: it has proper type system and local bindings, so it should be less painful.

[day 10]: ../day10/
