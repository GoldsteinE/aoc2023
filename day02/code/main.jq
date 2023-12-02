def parse_turn: [
    . | split(", ")[]
      | split(" ")
      | {key: .[1], value: (.[0] | tonumber)}
] | from_entries;

def parse_game: . | split(": ") | {
    game_no: (.[0] | split(" ") | (.[1] | tonumber)),
    turns: (.[1] | split("; ") | map(parse_turn)),
};

def is_game_valid: [.turns[] | .red <= 12 and .green <= 13 and .blue <= 14] | all;
def evaluate_game_part1: if (. | is_game_valid) then .game_no else 0 end;

def evaluate_game_part2: reduce .turns[] as $turn ({}; {
	red:   [.red,   $turn.red]   | max,
	green: [.green, $turn.green] | max,
	blue:  [.blue,  $turn.blue]  | max,
}) | .green * .red * .blue;

def evaluate_game: if $part == "2" then evaluate_game_part2 else evaluate_game_part1 end;

. | rtrimstr("\n") | split("\n") | map(parse_game | evaluate_game) | add
