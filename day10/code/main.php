<?

function processChange($px, $py, $x, $y, $nx, $ny) {
    list($dx, $dy, $dnx, $dny) = [$x - $px, $y - $py, $nx - $x, $ny - $y];
    $s = $dx * ($y + $py);
    if ($dnx == $dy && $dny == -$dx) {
        $corr = -0.75;
    } elseif ($dnx == -$dy && $dny == $dx) {
        $corr = -0.25;
    } else {
        $corr = -0.5;
    }
    return [$s, $corr];
}

$sketch = [];
$y = 0;
while ($line = fgets(STDIN)) {
    $line = str_split(rtrim($line));
    $sketch[] = $line;
    $x = 0;
    foreach ($line as $char) {
        if ($char == 'S') {
            $start = [$x, $y];
        }
        $x += 1;
    }
    $y += 1;
}

list($x, $y) = $start;
list($px, $py) = $start;
list($fx, $fy) = [null, null];
$s = 0;
$corr = 0;
$explored = array("$x:$y" => 0);
$to_explore = [];

if (preg_match('/\||L|J/', $sketch[$y + 1][$x])) {
    $to_explore[] = [$x, $y + 1, 0];
} elseif (preg_match('/-|7|J/', $sketch[$y][$x + 1])) {
    $to_explore[] = [$x + 1, $y, 0];
}

list($fx, $fy) = $to_explore[0];

while (!empty($to_explore)) {
    list($x, $y, $dist) = array_pop($to_explore);
    /**/if ($sketch[$y][$x] == '|') { $neighbours = [[$x, $y - 1], [$x, $y + 1]]; }
    elseif ($sketch[$y][$x] == '-') { $neighbours = [[$x - 1, $y], [$x + 1, $y]]; }
    elseif ($sketch[$y][$x] == 'L') { $neighbours = [[$x, $y - 1], [$x + 1, $y]]; }
    elseif ($sketch[$y][$x] == 'J') { $neighbours = [[$x, $y - 1], [$x - 1, $y]]; }
    elseif ($sketch[$y][$x] == '7') { $neighbours = [[$x - 1, $y], [$x, $y + 1]]; }
    elseif ($sketch[$y][$x] == 'F') { $neighbours = [[$x + 1, $y], [$x, $y + 1]]; }

    foreach ($neighbours as $neighbour) {
        list($nx, $ny) = $neighbour;
        if (!array_key_exists("$nx:$ny", $explored)) {
            list($ds, $dcorr) = processChange($px, $py, $x, $y, $nx, $ny);
            $s += $ds;
            $corr += $dcorr;
            $to_explore[] = [$nx, $ny, $dist + 1];
        }
    }
    $explored["$x:$y"] = $dist + 1;
    list($ppx, $ppy) = [$px, $py];
    list($px, $py) = [$x, $y];
}

if ($argv[1] == "1") {
    echo ceil($explored["$x:$y"] / 2), "\n";
} else {
    list($ds, $dcorr) = processChange($ppx, $ppy, $x, $y, $start[0], $start[1]);
    $s += $ds;
    $corr += $dcorr;

    list($ds, $dcorr) = processChange($px, $py, $start[0], $start[1], $fx, $fy);
    $s += $ds;
    $corr += $dcorr;

    echo abs(abs($s) / 2 + $corr), "\n";
}

# vim: ts=4 sw=4 et
