```ucm:hide
.> builtins.merge
.> pull @unison/base/releases/2.9.1 lib.base_2_9_1
```

```unison
unique type Task = {
	record : Text,
	sequence : [Nat],
	prevBroken : Nat,
}

countWays : (Task ->{g} Nat) -> Task ->{g} Nat
countWays rec task =
	use Nat < == != +
	use Boolean and

	record = (Task.record task)
	sequence = (Task.sequence task)
	prevBroken = (Task.prevBroken task)

	match List.uncons sequence with
		None -> if Text.contains "#" record then 0 else 1
		Some (head, tail) -> match Text.uncons record with
			None -> 0
			Some (c, rest) -> match c with
				?# | prevBroken == head ->
					0
				?# ->
					rec (Task rest sequence (prevBroken + 1))
				?? | and (prevBroken != 0) (prevBroken < head) ->
					rec (Task rest sequence (prevBroken + 1))
				?? | prevBroken == head ->
					rec (Task rest tail 0)
				?? ->
					(
						rec (Task rest sequence 1) +
						rec (Task rest sequence 0)
					)
				_ | prevBroken == head ->
					rec (Task rest tail 0)
				_ | prevBroken == 0 ->
					rec (Task rest sequence 0)
				_ -> 0

cached : mutable.ByteArray.Raw g -> Nat -> ((Task ->{g, Exception} Nat) -> Task ->{g, Exception} Nat) -> Task ->{g, Exception} Nat
cached arr maxTextLength f task =
	use Nat * +
	use mutable.ByteArray.Raw write64be read64be

	idx = 8 * (
		(Text.size (Task.record task)) +
		(maxTextLength * (
			(Task.prevBroken task) +
		 	(maxTextLength * (List.size (Task.sequence task)))
		))
	)

	match read64be arr idx with
		0xffffffffffffffff ->
			res = f (cached arr maxTextLength f) task
			write64be arr idx res
			res
		val -> val

parseLine : Nat -> Text ->{Exception} Task
parseLine part line =
	x5 sep text =
		if part == 2 then
			Text.join sep [text, text, text, text, text]
		else text

	match Text.words line with
		[record, raw_sequence] ->
			record' = Text.join "" [(x5 "?" record), "."]
			raw_sequence' = Text.split ?, (x5 "," raw_sequence)
			sequence = List.map Nat.fromTextOrFail raw_sequence'
			Task record' sequence 0
		_ -> Exception.raise (failure "wrong format" line)

main : '{IO, Exception} ()
main _ =
	use Nat * +

	part = match getArgs () with
		["1"] -> 1
		_     -> 2

	inputLines = Text.lines (Text.trim (getAllText stdIn))
	tasks = List.map (parseLine part) inputLines

	countWaysWithCache task = Scope.run do
		textLength = Text.size (Task.record task)
		sequenceLength = List.size (Task.sequence task)
		cacheSize = (textLength + 1) * (textLength + 1) * (sequenceLength + 1)
		cache = Scope.Raw.byteArrayOf 0xff (cacheSize * 8)
		cached cache textLength countWays task

	ways = List.map countWaysWithCache tasks
	result = List.foldLeft (Nat.+) 0 ways
	printLine (Nat.toText result)
```

```ucm
.> add
```
