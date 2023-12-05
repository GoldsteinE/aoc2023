open List

type rule = {
    src_start : int;
    dst_start : int;
    size : int;
}

type range = {
    start : int;
    size : int;
}

type task = {
    seeds : int list;
    maps : (rule list) list;
}

let try_match v r =
    let { src_start; dst_start; size; } = r in 
    if v > src_start && v < src_start + size then
        Some (dst_start - src_start + v)
    else
        None

let try_match_range ra ru =
    let ({ start; size; }, { src_start; dst_start; size = rule_size; }) = (ra, ru) in
    let matched_start = max start src_start in
    let matched_end = min (start + size) (src_start + rule_size) in
    if matched_start < matched_end then
        let matched_part = {
            start = dst_start - src_start + matched_start;
            size = matched_end - matched_start;
        } in
        let rest = filter (fun r -> r.size > 0) [
            { start; size = matched_start - start; };
            { start = matched_end; size = start + size - matched_end;  }
        ] in
        Some (matched_part, rest)
    else None

let solve_part1 t =
    let find_location t seed =
        let go curr rules = find_map (try_match curr) rules |> Option.value ~default:curr
        in fold_left go seed t.maps
    in
    t.seeds |> map (find_location t) |> fold_left min max_int

let solve_part2 t =
    let rec to_ranges xs = match xs with
    | [] -> []
    | start :: size :: rest -> { start; size; } :: to_ranges rest
    | _ -> failwith "seeds line must have even amount of numbers"
    in
    let ranges = to_ranges t.seeds in
    let go ranges rules =
        (* I don't see a nice functional solution here, so I'll just fall back to imperative code. *)
        let ranges = ref ranges in
        let mapped_ranges = ref [] in
        while not (compare_length_with !ranges 0 = 0) do
            let try_split_range (ready, left) range =
                match find_map (try_match_range range) rules with
                | Some (matched, rest) -> (matched :: ready, rest @ left)
                | None -> (range :: ready, left)
            in
            let (ready, left) = fold_left try_split_range ([], []) !ranges in
            mapped_ranges := ready @ !mapped_ranges;
            ranges := left;
        done;
        !mapped_ranges
    in
    let final_ranges = fold_left go ranges t.maps in
    final_ranges |> map (fun r -> r.start) |> fold_left min max_int

let parse_int_list inp = String.split_on_char ' ' inp |> map int_of_string

let parse_rule inp =
    match parse_int_list inp with
    | [dst_start; src_start; size] -> { dst_start; src_start; size }
    | _ -> failwith "can't parse rule"

let parse_rules inp = String.split_on_char '\n' inp |> tl |> map parse_rule

let parse_task inp =
    let parts = Str.(split (regexp "\n\n")) inp in
    match parts with
    | seeds_part :: rules_parts ->
        (match Str.(split (regexp ": ")) seeds_part with
        | [ _seeds_header; seeds_nums ] -> {
            seeds = parse_int_list seeds_nums;
            maps = map parse_rules rules_parts;
        }
        | _ -> failwith "invalid seeds section")
    | _ -> failwith "not enough sections"

let main () = 
    let task = In_channel.(input_all stdin) |> String.trim |> parse_task in
    let result = (if Sys.argv.(1) = "1" then solve_part1 else solve_part2) task in
    Printf.printf "%d\n" result

let () = main ()
