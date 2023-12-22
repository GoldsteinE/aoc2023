# Day 20: GNU Smalltalk

This was once again one of these tasks that are impossible to solve in general, only for a specific input.

At least this was kind of fun: I visualized the system and realized that there’re basically 4 independent
half-broken binary counters and product (actually LCM, but they’re coprime anyway) of their cycle sizes is the answer.

(Part of) visualization is included below, but the code I used to generate it is not, since it’s not in Smalltalk.
I’ve done it by basically generating a lot of `.dot` files, graphvizing them all and ffmpeging thousands of PNGs into a video.

![A visualization of modules graph](visualization.mp4)
