.intel_syntax noprefix

.section .data
# we need to read our input somewhere
buffer: .fill 128
# separate buffer for formatting & concatenation in part 2
number_buffer: .fill 32

.section .text

# type: () -> (size in rax, clobbers everything)
# read stdin to `buffer`
read_input:
    mov r12, offset buffer
    mov r13, 128     # buffer size
.L_read_loop:
    xor rax, rax     # syscall number = 0 (read)
    xor rdi, rdi     # fd = 0 (stdin)
    mov rsi, r12     # buffer pointer
    mov rdx, r13     # how many bytes to read
    syscall
    add r12, rax     # move the pointer
    sub r13, rax     # less bytes left
    test rax, rax    # is it EOF?
    jnz .L_read_loop # if not, continue
    mov rax, r12     # put buffer pointer in rax
    # and calculate length:
    sub rax, offset buffer
    ret

# type: (buffer in r13) -> (buffer in r13, clobbers rcx)
# move buffer pointer right until it points to a non-space character
skip_spaces:
    mov ch, 32            # a space
    mov cl, [r13]         # current character
    cmp ch, cl            # if not equals
    jne .L_spaces_skipped # exit function
    inc r13               # else move pointer
    jmp skip_spaces       # and repeat
.L_spaces_skipped:
    ret

# type: (buffer in r13, char in ah) -> (buffer in r13, clobbers rcx)
# move buffer pointer right past the first `char`
skip_until:
    mov cl, [r13]   # load current character
    inc r13         # move pointer
    cmp ch, cl      # if current character does not match
    jne skip_until  # repeat
    ret             # else return

# type: (buffer in r13) -> (buffer in r13, clobbers rcx)
# move buffer pointer right past the first `:` and then skips spaces
skip_header:
    mov ch, 58      # a colon
    call skip_until # skip until after the first colon
    jmp skip_spaces # then skip spaces

# type: (buffer in memory) -> (times in r14, distances in r15, clobbers rcx and r15)
# split `buffer` into times part and distances part, stripping headers
split_buffer:
    mov r13, offset buffer
    call skip_header # skip the `Time:` header
    mov r14, r13     # save the first line
    mov ch, 10       # a newline character
    call skip_until  # go to next line
    call skip_header # skip the `Distance:` header
    mov r15, r13     # save the second line
    ret

# type: (number in rax) -> (pointer to buffer in rsi, length in rdx, clobbers rcx, number_buffer)
# format a decimal number + newline to number_buffer and return pointer + length
format_number:
    # load pointer to the end of the buffer:
    lea rsi, [number_buffer + 31]
    movb [rsi], 10 # terminate with newline
    mov rcx, 10    # to divide number
.L_fn_loop:
    dec rsi        # move pointer left
    xor rdx, rdx   # idiv takes 128-bit number with high bits in rdx
    idiv rcx       # divide by 10 (result in rax, remainder in rdx)
    add dl, 48     # add '0' to the remainder
    mov [rsi], dl  # and put the digit into the buffer
    test rax, rax  # if number is non-zero
    jnz .L_fn_loop # repeat
    # load pointer to the end of the buffer into rdi (+1 for newline):
    lea rdx, [number_buffer + 32]  
    sub rdx, rsi   # and subtract current pointer
    ret

# type: (start pointer in r12, past-the-end pointer in r13) -> (number in rax, clobbers rcx and rdx)
# parse a number from buffer
parse_number:
    xor rax, rax      # number = 0
    mov rcx, 1        # current multiplier
.L_pn_loop:
    dec r13           # move end pointer left
    cmp r13, r12      # if passed start
    jl .L_pn_end      # finish
    movzxb rdx, [r13] # load a digit
    sub rdx, 48       # subtract '0'
    imul rdx, rcx     # scale by multiplier
    add rax, rdx      # add to result
    imul rcx, 10      # step multiplier
    jmp .L_pn_loop    # repeat
.L_pn_end:
    ret

# type: (buffer in r13) -> (buffer in r13, number in rax, clobbers r12, rbx, rcx and rdx)
# take a number from a buffer, terminating on space or newline, skip trailing spaces
take_number:
    mov r12, r13      # save the original pointer
    mov ch, 32        # a space
    mov dh, 10        # a newline
.L_tn_loop:
    mov cl, [r13]     # load a byte
    cmp cl, ch        # if it's a space
    je .L_tn_end      # go to the end
    cmp cl, dh        # if it's a newline
    je .L_tn_end      # go to the end
    inc r13           # else move pointer
    jmp .L_tn_loop    # and repeat
.L_tn_end:
    mov rbx, r13      # temporarily save r13
    call parse_number # parse a number in r12..r13
    mov r13, rbx      # restore r13
    call skip_spaces  # skip trailing spaces (if we're on newline, this does nothing)
    ret

# type: (times in r14, distances in r15) -> (same, time in rax, distance in rbx, clobbers r12, rsi, rcx and rdx)
# take time from `times`, distance from `distances`, move both pointers and return both values
take_race:
    mov r13, r14     # load `times` pointer
    call take_number # take time
    mov r14, r13     # store new `times` pointer
    mov rsi, rax     # save the time
    mov r13, r15     # load `distances` pointer
    call take_number # take distance
    mov r15, r13     # store new `distances` pointer
    mov rbx, rax     # put `distance` into return register
    mov rax, rsi     # put `time` into return register
    ret

# type: (buffer in r10) -> (number in number_buffer, past-the-end pointer in r13, clobbers rbx and rdx)
# concatenate all space-separated parts of the number until newline
concat_number:
    mov bh, 10     # a newline
    mov dh, 32     # a space
    # load target buffer pointer:
    mov r13, offset number_buffer 
.L_cn_loop:
    mov bl, [r10]  # load a byte
    cmp bl, dh     # if it's a space
    je .L_cn_cont  # continue
    cmp bl, bh     # if it's a newline
    je .L_cn_end   # return
    mov [r13], bl  # else store a byte into the target buffer
    inc r13        # and move target pointer right
.L_cn_cont:
    inc r10        # move source pointer right
    jmp .L_cn_loop # repeat
.L_cn_end:
    ret

# type: (time in rax, distance in rbx) -> (count in rcx, clobbers rsi, rdi, rdx)
# count ways to win a race
ways_to_win:
    xor rcx, rcx    # counter = 0
    mov rdx, rax    # rdx is "button holding time"
.L_ww_loop:
    mov rsi, rax    # rsi is "moving time"
    sub rsi, rdx    # while we hold the button, we don't move
    xor rdi, rdi    # rdi is whether we need to increase the counter
    imul rsi, rdx   # now rsi is total distance
    cmp rsi, rbx    # compare it with required distance
    mov rsi, 1      # now we can re-use rsi to hold 1 for cmov
    cmovg rdi, rsi  # if succeeds, increase the counter
    add rcx, rdi    # (actually do it)
    sub rdx, 1      # decrease the button holding time
                    # dec wouldn't work here, because it doesn't touch carry flag
    jnc .L_ww_loop  # if we didn't underflow, repeat
    ret             # else return

# type: (times in r14, distances in r15) -> (answer in r8, clobbers everything)
part1:
    mov r8, 1        # initialize answer
    mov r9w, 10      # newline (in 16-bit register, because r9 doesn't have 8-bit one)
.L_p1_loop:
    call take_race   # take two numbers from buffers...
    call ways_to_win # process them...
    imul r8, rcx     # and add answer to the result
    movzxb cx, [r14] # load current `times` byte
    cmp cx, r9w      # if we didn't reach the end yet
    jne .L_p1_loop   # repeat
    ret              # else return

# type: (times in r14, distances in r15) -> (answer in r8, clobbers everything)
part2:
    mov r10, r15       # load distances into r10
    call concat_number # concatenate them into one number
    # load number buffer into r12:
    mov r12, offset number_buffer
    call parse_number  # parse distance to rax
    mov rsi, rax       # temporarily save it to rsi
    mov r10, r14       # load times into r10
    call concat_number # concatenate them into one number
    # load number buffer into r12:
    mov r12, offset number_buffer
    call parse_number  # parse time to rax
    mov rbx, rsi       # put distances to rbx
    call ways_to_win   # calculate answer
    mov r8, rcx        # put it into return register
    ret

.global _start
_start:
    call read_input
    call split_buffer

    mov rdi, offset part1 # load part 1 function into rdi
    mov rsi, offset part2 # load part 2 function into rsi
    mov rax, [rsp]        # load arg count
    cmp rax, 1            # if arg count is not 1
    cmovne rdi, rsi       # put part 2 function into rdi
    call rdi              # call chosen part function

    mov rax, r8           # put result into rax
    call format_number    # format it
    mov rax, 1            # syscall number (write)
    mov rdi, 1            # file descriptor (stdout)
    syscall

    mov rax, 60           # syscall number (exit)
    mov rdi, 0            # exit code
    syscall

# vim: sw=4 ts=4 et
