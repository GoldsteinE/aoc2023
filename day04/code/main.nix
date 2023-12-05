{ part, filename }:

let
  std = import (builtins.fetchGit {
    url = "https://github.com/chessai/nix-std.git";
    rev = "715db541ffff4194620e48d210b76f73a74b5b5d";
  });

  inherit (builtins) any filter map readFile elemAt length foldl' concatMap;
  inherit (std.num) pow parseInt;
  inherit (std.regex) splitOn;
  inherit (std.string) lines strip;

  inputLines = lines (readFile filename);

  # Quadratic, but whatever.
  intersect = xs: ys: filter (x: any (y: x == y) ys) xs;

  parseCard = raw:
    let
      bigParts = splitOn ": " raw;
      cardNoPart = elemAt bigParts 0;
      cardNo = parseInt (elemAt (splitOn " +" cardNoPart) 1);
      numberParts = map strip (splitOn " \\| " (elemAt bigParts 1));
      winning = map parseInt (splitOn " +" (elemAt numberParts 0));
      present = map parseInt (splitOn " +" (elemAt numberParts 1));
    in
    rec {
      inherit cardNo winning present;
      winningCount = length (intersect winning present);
      points = if winningCount == 0 then 0 else pow 2 (winningCount - 1);
    };

  cards = map parseCard inputLines;

  part1 = foldl' (acc: card: acc + card.points) 0 cards;

  part2 =
    let
      stepInfluences = concatMap (influence: if influence.lifetime <= 1 then [ ] else [{
        inherit (influence) power;
        lifetime = influence.lifetime - 1;
      }]);
      initial = {
        score = 0;
        influences = [ ];
      };
      calcScore = influences: 1 + foldl' (acc: influence: acc + influence.power) 0 influences;
      step = { score, influences }: card:
        let power = calcScore influences; in {
          score = score + power;
          influences = (stepInfluences influences) ++ (if card.winningCount != 0 then [{
            # Ew, inherited power!
            inherit power;
            lifetime = card.winningCount;
          }] else [ ]);
        };
    in
    (foldl' step initial cards).score;

in
if part == 1 then part1 else part2
