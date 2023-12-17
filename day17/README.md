# Day 17: Zig

I chose Zig because it has bit-packed structs, which allowed me to easily replace hashmap with an array for this sweet sweet performance boost.
This is [Hard Mode Zig]: I only ever allocate three arrays at the start of `main()`.

I like Zig. Mixing comptime and runtime code feels really fun, and the language is quite ergonomic.
The solution is still pretty long, mostly because of utilities for dealing with arrays and directions.

[Hard Mode Zig]: https://matklad.github.io/2022/10/06/hard-mode-rust.html
