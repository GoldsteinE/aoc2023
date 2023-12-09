#!/bin/sh

# shellcheck disable=SC2154

set -eu

part="${1:-1}"

step_sequence() {
	prev=''
	next_line=''
	over=1
	for num in $line; do
		diff="$((num - prev))"
		if [ -n "$prev" ]; then
			next_line="${next_line}${diff} "
			if [ "$diff" -ne 0 ]; then
				over=0
			fi
		fi
		prev="$num"
	done
	line_idx=$((line_idx + 1))
	eval "line_${line_idx}"="'${next_line}'"
	line="${next_line}"
}

do_change() {
	is_first=1
	for num in $line; do
		if [ "$is_first" -eq 1 ]; then
			change_part2=$((num - change_part2))
			is_first=0
		fi
	done
	change_part1=$((num + change_part1))
}

result_part1=0
result_part2=0

while read -r line; do
	# used dynamically in step_sequence
	# shellcheck disable=SC2034
	line_0="${line}"
	line_idx=0

	over=0
	while [ "$over" = 0 ]; do
		step_sequence
	done

	change_part1=0
	change_part2=0
	line_idx=$((line_idx - 1))  # skip zero-filled line
	while [ "$line_idx" -ge 0 ]; do
		eval line='$line_'"$line_idx"
		do_change
		line_idx=$((line_idx - 1))
	done
	result_part1=$((result_part1 + change_part1))
	result_part2=$((result_part2 + change_part2))
done

if [ "$part" = "1" ]; then
	echo "$result_part1"
else
	echo "$result_part2"
fi
