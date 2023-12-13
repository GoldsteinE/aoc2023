class Guard {}

sub who-breaks-reflection(List $row, Int $at) {
    my $res = Set[Int].new;
    my $left = $at - 1;
    my $right = $at;
    while ($left ≥ 0 && $right < $row.elems) {
        if $row[$left] ne $row[$right] {
            if ($res.elems ≠ 0) {
                return Guard.new;
            }
            $res = set $left, $right;
        }
        $left--;
        $right++;
    }
    $res
}

sub find-mirrors(Seq $map --> Set[Int]) {
    reduce -> $acc, $row {
        $acc ∩ set grep { who-breaks-reflection($row, $^a) ≡ set() }, $acc.keys
    }, (Set[Int].new: 1 ..^ $map[0].elems), |$map;
}

sub find-almost-mirrors(Seq $map --> Set[Int]) {
    my $all = Set.new(((0 ..^ $map.elems) X (0 ..^ $map[0].elems)).map({ $_[0] => $_[1] }));
    Set[Int].new: ((1 ..^ $map[0].elems).map(-> $at {
        my $breaks = [∩] $all, |$map.pairs.map(-> $row {
            my $res = who-breaks-reflection($row.value, $at);
            $res ?? Set.new($res.keys.map({ $row.key => $_ })) !! Empty
        });
        $breaks.elems == 2 ?? $at !! Empty
    }))
}

sub MAIN(Int $part) {
    my $func = $part == 1 ?? &find-mirrors !! &find-almost-mirrors;
    my @maps = $*IN.slurp.chomp.split("\n\n").map(*.lines.map(*.split("", :skip-empty).list));
    say @maps.map({ $func($_).keys.sum + 100 * $func([Z] $_).keys.sum }).sum;
}

# vim: ts=4 sw=4 et
