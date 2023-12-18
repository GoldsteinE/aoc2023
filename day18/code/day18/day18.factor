USING:
    kernel namespaces
    lists arrays sequences combinators
    math math.parser math.vectors
    prettyprint splitting
    io io.files io.encodings.utf8 command-line ;
IN: day18

: direction>offset ( ch -- dx dy ) {
    { [ dup dup CHAR: R = swap CHAR: 0 = or ] [  1  0 ] }
    { [ dup dup CHAR: D = swap CHAR: 1 = or ] [  0  1 ] }
    { [ dup dup CHAR: L = swap CHAR: 2 = or ] [ -1  0 ] }
    { [ dup dup CHAR: U = swap CHAR: 3 = or ] [  0 -1 ] }
} cond rot drop ;

: parse-part1 ( line -- direction count )
    " " split first3 drop string>number ;

: parse-part2 ( line -- direction count )
    " " split last { 2 7 8 } split-indices first3 nipd swap hex> ;

: find-vertices ( lines parse-line -- vertices )
    [
        swap first direction>offset
        pick * -rot *
        rot first2 -roll + -rot + 2array
    ] compose { 0 0 } swap accumulate swap suffix ; inline

: compute-area ( vertices -- area )
    0 swap dup first first2 rot 1 tail [
        first2 4dup swapd + -rot - *
        -rot 2array -rotd swap dupd rotd 2array distance
        -rotd + + swap first2
    ] each 2drop 2 / 1 + >integer ;

: read-input ( -- lines )
    input-stream get stream-contents
    "\n" split [ "" = ] reject ;

: main ( -- )
    read-input
    "part" get "1" =
        [ [ parse-part1 ] find-vertices ]
        [ [ parse-part2 ] find-vertices ]
    if
    compute-area . ;

MAIN: main

! vim: ts=4 sw=4 et
