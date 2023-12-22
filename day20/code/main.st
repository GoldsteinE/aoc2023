Object subclass: Signal [
    | sender receiver pulse |
    Signal class >> fromPulse: pls sender: snd receiver: rcv [
        | r |
        r := Signal new.
        r initWithPulse: pls sender: snd receiver: rcv.
        ^r
    ]
    initWithPulse: pls sender: snd receiver: rcv [
        sender := snd.
        receiver := rcv.
        pulse := pls.
    ]
    sender [
        ^sender
    ]
    receiver [
        ^receiver
    ]
    pulse [
        ^pulse
    ]
]

Object subclass: Module [
    | addr inputs outputs |
    init: a [
        addr := a.
        inputs := OrderedCollection new.
        outputs := OrderedCollection new.
    ]
    registerInput: a [
        inputs add: a.
    ]
    registerOutput: a [
        outputs add: a.
    ]
    hasOutput: output [
        | ans |
        ans := false.
        outputs do: [:receiver |
            ans := ans | (receiver = output).
        ].
        ^ans
    ]
    outputsDo: block [
        outputs do: block.
    ]
    broadcast: pulse [
        | res signal |
        res := OrderedCollection new.
        outputs do: [:receiver |
            signal := Signal fromPulse: pulse sender: addr receiver: receiver.
            res add: signal.
        ].
        ^res
    ]
    process: signal [
        ^OrderedCollection new.
    ]
]

Module subclass: Broadcast [
    process: signal [
        ^self broadcast: (signal pulse).
    ]
]

Module subclass: FlipFlop [
    | state |
    init: a [
        super init: a.
        state := false.
    ]
    process: signal [
        ((signal pulse) = 'low') ifTrue: [
            state := state not.
            ^self broadcast: (state ifTrue: [ 'high' ] ifFalse: [ 'low' ])
        ] ifFalse: [
            ^OrderedCollection new.
        ]
    ]
]

Module subclass: Conjunction [
    | lastPulse |
    init: a [
        super init: a.
        lastPulse := Dictionary new.
    ]
    registerInput: a [
        super registerInput: a.
        lastPulse at: a put: 'low'.
    ]
    process: signal [
        | high |
        lastPulse at: (signal sender) put: (signal pulse).
        high := true.
        lastPulse do: [:value|
            high := high & (value = 'high')
        ].
        ^self broadcast: (high ifTrue: [ 'low' ] ifFalse: [ 'high' ]).
    ]
]

Object subclass: System [
    | modules totalLow totalHigh precursorNodes |
    init: m [
        | collector |
        modules := m.
        totalLow := 0.
        totalHigh := 0.
        precursorNodes := Dictionary new.

        modules keysAndValuesDo: [:addr :module|
            (module hasOutput: 'rx') ifTrue: [ collector := addr ].
        ].
        modules keysAndValuesDo: [:addr :module|
            (module hasOutput: collector) ifTrue: [ precursorNodes at: addr put: nil ].
        ].
    ]
]

Object subclass: Parser [
    | modules |
    init [
        modules := Dictionary new.
    ]
    parseLine: line [
        | lineParts spec addr receivers moduleType module |
        lineParts := line tokenize: ' -> '.
        spec := lineParts at: 1.
        receivers := lineParts at: 2.
        (spec = 'broadcaster') ifTrue: [
            addr := spec.
            moduleType := Broadcast.
        ] ifFalse: [
            addr := spec copyFrom: 2.
            spec := spec copyFrom: 1 to: 1.
            (spec = '%') ifTrue: [ moduleType := FlipFlop ].
            (spec = '&') ifTrue: [ moduleType := Conjunction ].
        ].
        module := moduleType new.
        module init: addr.
        (receivers tokenize: ', ') do: [:receiver|
            module registerOutput: receiver.
        ].
        modules at: addr put: module.
    ]
    finalize [
        | system |
        modules keysAndValuesDo: [:addr :module|
            module outputsDo: [:output|
                (modules includesKey: output) ifTrue: [
                    (modules at: output) registerInput: addr.
                ].
            ].
        ].
        system := System new.
        system init: modules.
        ^system
    ]
]

System extend [
    precursorNodes [
        | collector res |
        modules keysAndValuesDo: [:addr :module|
            (module hasOutput: 'rx') ifTrue: [ collector := addr ].
        ].
        res := OrderedCollection new.
        modules keysAndValuesDo: [:addr :module|
            (module hasOutput: collector) ifTrue: [ res add: addr ].
        ].
        ^res
    ]
    simulate: n [
        | low high todo signal foundPart2 |
        low := 0.
        high := 0.
        todo := OrderedCollection new.
        todo add: (Signal fromPulse: 'low' sender: 'button' receiver: 'broadcaster').
        [ (todo size) ~= 0 ] whileTrue: [
            signal := todo removeFirst.
            ((signal pulse) = 'low') ifTrue: [
                low := low + 1.
            ] ifFalse: [
                high := high + 1.
                (precursorNodes includesKey: (signal sender)) ifTrue: [
                    precursorNodes at: (signal sender) put: n.
                ].
            ].
            (modules includesKey: (signal receiver)) ifTrue: [
                ((modules at: (signal receiver)) process: signal) do: [:signal|
                    todo add: signal.
                ].
            ].
        ].

        totalLow := totalLow + low.
        totalHigh := totalHigh + high.

        foundPart2 := true.
        precursorNodes do: [:cycle|
            foundPart2 := foundPart2 & (cycle ~= nil).
        ].
        ^foundPart2
    ]
    part1 [
        ^(totalLow * totalHigh)
    ]
    part2 [
        |res|
        res := 1.
        precursorNodes do: [:cycle| res := res * cycle ].
        ^res
    ]
]

parser := Parser new.
parser init.
stdin linesDo: [:line | parser parseLine: line ].
system := parser finalize.

part := (Smalltalk arguments) at: 1 ifAbsent: [].

(part = '1') ifTrue: [
    1000 timesRepeat: [ system simulate: 0 "n doesn't matter here" ].
    (system part1) printNl.
] ifFalse: [
    n := 1.
    [ system simulate: n ] whileFalse: [ n := n + 1 ].
    (system part2) printNl.
]

"vim: set ft=st ts=4 sw=4 et:"
