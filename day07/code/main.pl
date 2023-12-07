card_rank(0'T, 10).
card_rank(0'Q, 12). % (sic! 11 is J in part 1)
card_rank(0'K, 13).
card_rank(0'A, 14).
card_rank(0'J, N) :- b_getval(aoc_part, 1), N = 11;
                     b_getval(aoc_part, 2), N = 1.
card_rank(Chr, N) :- between(0'2, 0'9, Chr), N is Chr - 0'0.

card_eq(Cards) :- list_to_set(Cards, [_]); % This works for both part 1 and 2.
                  b_getval(aoc_part, 2), list_to_set(Cards, [A, B]), member(0'J, [A, B]).

% There're no empty combinations.
combination(_, []).
% On every step we can either take a value from the list...
combination([X | Xs], [X | Ys]) :- combination(Xs, Ys).
% ...or don't.
combination([_ | Xs], [Y | Ys]) :- combination(Xs, [Y | Ys]).

has_group(Hand, Num) :- combination(Hand, Group), length(Group, Num), card_eq(Group).
has_two_pairs(Hand)  :- combination(Hand, Four), msort(Four, [A, B, C, D]), card_eq([A, B]), card_eq([C, D]).

has_full_house_sorted([A, B, C, D, E]) :- card_eq([A, B, C]), card_eq([D, E]).
has_full_house_sorted([A, B, C, D, E]) :- card_eq([C, D, E]), card_eq([A, B]).
has_full_house(Hand) :- msort(Hand, Sorted), has_full_house_sorted(Sorted).

hand_rank_points(Hand, Points) :-
    card_eq(Hand)        -> Points = 6;
    has_group(Hand, 4)   -> Points = 5;
    has_full_house(Hand) -> Points = 4;
    has_group(Hand, 3)   -> Points = 3;
    has_two_pairs(Hand)  -> Points = 2;
    has_group(Hand, 2)   -> Points = 1;
                            Points = 0.

hand_order((Hand, Bid), ((Points, CardRanks), Bid)) :-
    hand_rank_points(Hand, Points),
    maplist(card_rank, Hand, CardRanks).

sort_hands(Hands, Res) :-
    maplist(hand_order, Hands, Orders),
    msort(Orders, Res).

enumerate([], [], _).
enumerate([ X | Xs ], [ (Idx, X) | Ys ], Idx) :- enumerate(Xs, Ys, Idx + 1).

solve(Hands, Res) :-
    sort_hands(Hands, Sorted),
    enumerate(Sorted, WithRanks, 1),
    maplist([(Rank, (_, Bid)), Points]>>(Points = Rank * Bid), WithRanks, Winnings),
    sumlist(Winnings, Res).

parse_row(row(Cards, Bid), (Parsed, Bid)) :- string_codes(Cards, Parsed).

main :-
    getenv("INPUT", FileName),
    getenv("PART", RawPart),
    atom_number(RawPart, Part),
    b_setval(aoc_part, Part),
    csv_read_file(FileName, Lines, [separator(32)]),
    maplist(parse_row, Lines, ParsedInput),
    solve(ParsedInput, Result),
    format(user_output, '~d\n', Result).

% vim: ft=prolog ts=4 sw=4 et
