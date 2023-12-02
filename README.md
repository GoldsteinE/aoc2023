# Advent of Code 2023: 1/25 langs

I’ll try to solve this Advent of Code using different language for each day.
Any programs needed to run the code will be available via dev shell in the `flake.nix`.

## Languages

| Day | Language                 | Link                 |
| :-: | ------------------------ | -------------------- |
|  1  | BQN                      | [`./day01/`](/day01) |

## `check.sh`

You can use `check.sh` script in the root directory of the repo to run all tests for one or every day.

Working Rust installation is needed for this to work. Development shell in `flake.nix` provides one.

```
$ ./check.sh      # Run all tests for every available day
$ ./check.sh 2    # Run all tests for the second day
$ ./check.sh 1 3  # Run all tests for the first and the third day
```

This script will exit with non-zero status if any of the checks failed.

## Directory structure

In each `day*` directory there are following files and subdirectories:

|  Path                | Contents                                     |
| -------------------- | -------------------------------------------- |
| `./README.md`        | Description & various info                   |
| `./in/`              | Input files for the task                     |
| `./in/demo1.txt`     | Example for the first part                   |
| `./in/demo2.txt`     | Example for the second part                  |
| `./in/part1.txt`     | Input file for the first part                |
| `./in/part2.txt`     | Input file for the second part               |
| `./out/*.txt`        | Correct answers for the corresponding inputs |
| `./code/`            | Code of the solution                         |
| `./build/`           | Build artifacts                              |
| `./build/.keep`      | Empty file, required to commit `./build/`    |
| `./scripts/`         | Scripts for building & running the solution  |
| `./scripts/build.sh` | Build the solution                           |
| `./scripts/run.sh`   | Run the solution with specified input        |

Note that the actual inputs are only committed as an encrypted file, which is useless to you,
unless you’re me and have the password. You need to download and provide your own inputs.
You can use `fetch-inputs.sh` with your session cookie to do it.

## How to use `run.sh`

```
$ dayX/scripts/run.sh demo 1  # Run the first on the example
$ dayX/scripts/run.sh part 1  # Run the first part on the real input
$ dayX/scripts/run.sh demo 2  # Run the second part on the example
$ dayX/scripts/run.sh part 2  # Run the second part on the real input
```
