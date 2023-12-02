#!/bin/sh

set -xeu

if [ -z "${AOC_SESSION:-}" ]; then
	echo 'provide AOC_SESSION in an environment variable to run this script' >&2
	exit 1
fi

for dir in day*; do
	day="${dir#day}"
	url="https://adventofcode.com/2023/day/${day#0}/input"

	if ! [ -f "$dir/in/part1.txt" ]; then
		curl --silent --fail-early --output "$dir/in/part1.txt" -H "Cookie: session=$AOC_SESSION" "$url"
		cp "$dir/in/part1.txt" "$dir/in/part2.txt"
		# Try to be slower.
		sleep 2
	fi
done
