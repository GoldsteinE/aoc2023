# Day 4: Nix

I initially tried to solve it in SQLite (my attempt is commited as [`code/main.sql`](code/main.sql)), but I wasn't able to figure out how to do the right kind of recursion.

My Nix solution this year relies on [nix-std], which is an interesting no-nixpkgs “standard” library for Nix. I can’t be bothered to invent parsing helpers for Nix anymore.

[nix-std]: https://github.com/chessai/nix-std
