# Day 15: Nim

I solved this one in Nim because this is a task about moving lenses, and Nim is a game about moving rocks, so basically the same.

Nice things about Nim: it looks pretty clean, easy to write, a lot of utils in the standard library.

Not very nice thing about Nim: everything is a macro. Why is everyting a macro.
Why is `enumerate()` a macro that accepts a `for` loop and not like, an iterator, which is literally a first-class construct in Nim.

Also no pattern-matching without `fusion`, iterators are not composable (you can’t fold an iterator (did I mention that fold is also a macro?)), outparams are everywhere.
