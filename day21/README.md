# Day 21: C++

That once again was a task about looking at the input and discovering that it’s very fine-tuned, and then trying to guess what solution this fine-tuning is trying to imply.
It was not particularly fun. Once again I’m not sure that part 2 demo has a nice solution, because part 2 solution is not applicable to it.

I said the word “rhombus” more times than I ever thought I would.

I chose C++ for this one because it’s fast and even “smart” solution requires 300-ish iterations of propagation. Unfortunately, both `std::set` and `std::unordered_set` are anything but fast, so I’ve added Boost for `flat_set`.
The solution code itself is straightforward, the hard part was figuring out the math.
