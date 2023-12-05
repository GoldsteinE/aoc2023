-- Doesn't work, but maybe I can make it work?

with recursive

split_lines(line_no, line, text) as (
    select 0, null, readfile('/home/goldstein/pets/aoc2023/day04/in/demo1.txt')
    union all select
        line_no + 1,
        substr(text, 0, instr(text, x'0a')),
        substr(text, instr(text, x'0a') + 1)
    from split_lines where text != ''
),

card_parts(card_no, winning, present) as (
    select
        line_no,
        trim(substr(line, instr(line, ': ') + 2, instr(line, ' | ') - instr(line, ': ') - 2)),
        trim(substr(line, instr(line, '|') + 2))
    from split_lines
    where line != ''
    order by line_no asc
),

numbers_parse(card_no, kind, num, rest) as (
    select
        card_no, 'winning', null, winning || ' ' from card_parts
    union select
        card_no, 'present', null, present || ' ' from card_parts
    union all select
        card_no,
        kind,
        cast(trim(substr(rest, 0, instr(rest, ' '))) as int),
        ltrim(substr(rest, instr(rest, ' ') + 1))
    from numbers_parse where rest != ''
),

winning_numbers(card_no, num) as (
    select card_no, num from numbers_parse where kind = 'winning' and num is not null
),

present_numbers(card_no, num) as (
    select card_no, num from numbers_parse where kind = 'present' and num is not null
),

card_winning_counts(card_no, winning_count) as (
    select present_numbers.card_no, count(winning_numbers.num)
    from present_numbers
    left outer join winning_numbers on
            winning_numbers.card_no = present_numbers.card_no
        and winning_numbers.num     = present_numbers.num
    group by present_numbers.card_no
),

card_points(card_no, points) as (
    select card_no, cast(power(2, winning_count - 1) as int)
    from card_winning_counts
),

influences(origin, affected) as (
    select left_card.card_no as origin, right_card.card_no as affected
    from card_winning_counts as left_card
    join card_winning_counts as right_card
    where left_card.card_no < right_card.card_no
    and   right_card.card_no - left_card.card_no <= left_card.winning_count
),

card_counts(card_no, card_count, edges_left) as (
    select
        card_no, 1, count(origin)
    from card_winning_counts
    left outer join influences on affected = card_no
    group by card_no
)

select * from card_counts2;
