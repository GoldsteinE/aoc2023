       identification division.
       program-id. aoc2023-day08.
       data division.
       working-storage section.
      * for parsing:
       01 part pic x.
       01 actions pic x(1000).
       01 input-line pic x(32).
       01 input-rule.
           05 input-src pic x(3).
           05 input-dest-left pic x(3).
           05 input-dest-right pic x(3).
       01 rules-count pic 9999.
       01 rules-table occurs 0 to 1000 times
                      depending on rules-count
                      ascending key src
                      indexed by rules-idx.
           05 src pic x(3).
           05 dest-left pic x(3).
           05 dest-right pic x(3).
      * this probably should be a 88-level flag, but I'm too lazy to
      * figure out how that works.
       01 eof pic 9 value 0.

      * for both parts:
       01 steps pic 9(32) value 0.
       01 steps-display pic z(31)9.
       01 actions-length pic 9999.
       01 current-action-idx pic 9999 value 1.
       01 current-action pic x.
       01 current-position pic x(3) value "AAA".

      * for part 2:
       01 gcd pic 9(32).
       01 pointer-idx pic 9999.
       01 pointers-count pic 9999 value 0.
       01 pointers-left pic 9999.
       01 pointers-table occurs 0 to 1000 times
                         depending on pointers-count.
           05 pointer-start pic x(3).
           05 pointer-current pic x(3).
           05 cycle-size pic 9(16).
       procedure division.

      * parsing:
       accept actions
       inspect actions tallying actions-length for characters before
           initial space

      * skip an empty line:
       accept input-line
       perform until eof = 1
           accept input-line
               on exception move 1 to eof
           end-accept
           if eof = 0 then
               unstring input-line delimited by
                   all " = (" or all ", " or ")"
                   into input-src, input-dest-left, input-dest-right
               end-unstring
               add 1 to rules-count
               move input-src to src (rules-count)
               move input-dest-left to dest-left (rules-count)
               move input-dest-right to dest-right (rules-count)
     
               if input-src(3:1) = "A"
                   add 1 to pointers-count
                   move input-src to pointer-start(pointers-count)
                   move input-src to pointer-current(pointers-count)
                   move 0 to cycle-size(pointers-count)
               end-if
           end-if
       end-perform

       sort rules-table on ascending key src

       accept part from argument-value

       if part = "1"
           perform until current-position = "ZZZ"
               add 1 to steps
               move actions(current-action-idx:1) to current-action
               search all rules-table
                   when src(rules-idx) = current-position
                       if current-action = "L"
                           move dest-left(rules-idx)
                               to current-position
                       else
                           move dest-right(rules-idx)
                               to current-position
                       end-if
               end-search
     
               if current-action-idx = actions-length
                   move 1 to current-action-idx
               else
                   add 1 to current-action-idx
               end-if
           end-perform
       end-if

       if part = "2"
           move pointers-count to pointers-left
           perform until pointers-left = 0
               move actions(current-action-idx:1) to current-action
     
               perform varying pointer-idx from 1 by 1
                       until pointer-idx > pointers-count
                   move pointer-current(pointer-idx) to current-position
                   if pointer-current(pointer-idx) = spaces
                       exit perform cycle
                   end-if
     
                   search all rules-table
                       when src(rules-idx) = current-position
                           if current-action = "L"
                               move dest-left(rules-idx) to
                               pointer-current(pointer-idx)
                           else
                               move dest-right(rules-idx) to
                               pointer-current(pointer-idx)
                           end-if
                   end-search
                   add 1 to cycle-size(pointer-idx)
                   if pointer-current(pointer-idx)(3:1) = "Z"
                       subtract 1 from pointers-left
                       move spaces to pointer-current(pointer-idx)
                   end-if
               end-perform
     
               if current-action-idx = actions-length
                   move 1 to current-action-idx
               else
                   add 1 to current-action-idx
               end-if
           end-perform
     
           move cycle-size(1) to gcd
           move cycle-size(1) to steps
           perform varying pointer-idx from 2 by 1 until pointer-idx >
               pointers-count
               multiply steps by cycle-size(pointer-idx) giving steps
               perform until cycle-size(pointer-idx) = gcd
                   if cycle-size(pointer-idx) > gcd
                       subtract gcd from cycle-size(pointer-idx)
                   else
                       subtract cycle-size(pointer-idx) from gcd
                   end-if
               end-perform
               divide steps by gcd giving steps
           end-perform
       end-if

       move steps to steps-display
       display function trim (steps-display leading).
