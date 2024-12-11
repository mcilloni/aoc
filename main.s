.text
.globl _start

.macro round_to_page dest n
    // rounds n to the nearest multiple of 4096
    // clobbers x14, x15
    mov  x14, \n
    tst  x14, #0xFFF // check if the number is a multiple of 4096
    cset w15, ne     // set x1 to 1 if the number is not a multiple of 4096
    and  x14, x14, #0xFFFFFFFFFFFFF000 // round down to a multiple of 4096
    add  \dest, x14, x15, lsl #12 // add 4096, if the number was not a multiple of 4096. this is mmap's length
.endmacro

alloc:
    // x0 is the amount required (will be rounded up to 4096B pages)
    // returns the pointer to the allocated memory in x0 and the map size in x1
    round_to_page x1, x0 // round x0 to the nearest multiple of 4096 and store it in x1
    mov  x8, #222        // mmap syscall
    mov  x0, xzr         // addr = NULL
    mov  x2, #0b011      // prot = PROT_READ(1) | PROT_WRITE(2)
    mov  x3, #0b100010   // flags = MAP_PRIVATE(2) | MAP_ANONYMOUS(32)
    mov  x4, #-1         // fd = -1
    mov  x5, #0          // offset = 0
    svc  #0
    cmp  x0, #-1         // MAP_FAILED
    b.eq alloc.fail
    ret
alloc.fail:
    mov x0, xzr        // return NULL
    ret

atoull:
    // x0 is the pointer to the string
    // x1 is the length of the string
    // returns the number in x0, or 0 on error
    add x2, x0, x1 // calculate the end of the string
    mov x5, x1     // save the length
    mov x1, x0     // use x1 as iterator
    mov x0, xzr    // clear x0
    cbz x5, atoull.quit // if the string is empty, we're done
    mov x4, #10    // base 10
atoull.loop:
    ldrb w3, [x1], #1 // load the byte and increment the pointer
    sub w3, w3, #'0'  // check if it's a digit
    cmp w3, 10
    b.cs atoull.quit  // if the char is between ['0', '10'], (w3 - '0') < 10. Note, unsigned flag (carry set)
    madd x0, x0, x4, x3 // x0 = x0 * 10 + x3
    cmp x1, x2
    b.lo atoull.loop // if we're at the end of the string, we're done
atoull.quit:
    ret

close:
    // expect file descriptor in x0
    mov x8, #57 // close syscall
    svc #0
    ret

.macro copy64 dst src n
    // copies n bytes from src to dst in 64 bit chunks. n must be a multiple of 8 for _obvious_ reasons
    mov  x10, dst
    mov  x11, src
    add  x12, x11, n
    cmp  x11, x12
    b.hs copy64.quit\@
copy64.loop\@:
    ldp  x13, x14, [x11], #16
    stp  x13, x14, [x10], #16
    cmp  x11, x12
    b.lo copy64.loop\@
copy64.quit\@:
    
.endmacro

count_chunks:
    // x0 is the file descriptor
    // returns the number of chunks in x0, or negative on error
    stp x29, lr, [sp, #-16]! // store fp and lr to the stack
    mov x29, sp // set the frame pointer to the stack pointer
    stp xzr, xzr, [sp, #-16]! // `in_chunk` flag(1B) + pad + counter(8B)
    str x0, [sp, #-16]! // store the file descriptor on the stack (and realign sp)
count_chunks.loop:
    bl   read_byte
    cmp  x1, #0
    b.lt count_chunks.fail // x1 < 0, error
    b.gt count_chunks.done // x1 > 0, EOF
    bl   is_blank // check if the byte is a blank
    ldp  x1, x2, [x29, #-16] // load `in_chunk` and counter
    orr  x3, x0, x1
    cmp  x3, #0 // blank NOR in_chunk - if it's not a blank and we're not in a chunk, we're starting a new chunk
    cset x3, eq 
    add  x2, x2, x3 // increment the counter if we're starting a new chunk
    cmp  x0, #0     // !is_blank -> in_chunk
    cset x3, eq     // set in_chunk to 1 if we're not in a blank
    stp  x3, x2, [x29, #-16] // store `in_chunk` and counter
    ldur x0, [x29, #-32] // load the file descriptor for the next iteration
    b    count_chunks.loop
count_chunks.fail:
    mov x0, x1 // set x0 to the error code
    add sp, sp, #32 // discard the current frame 
    b   count_chunks.end
count_chunks.done:
    add sp, sp, #16   // discard the fd
    ldp xzr, x0, [sp], #16 // load the counter
count_chunks.end:
    ldp x29, lr, [sp], #16 // restore fp and lr
    ret

grow_map:
    // x0 is the pointer to old buffer (or NULL)
    // x1 is the old size (mmapped)
    // x2 is the new size (will be rounded to next map)
    // returns the pointer to the new buffer in x0, and the allocated size in x1, or NULL on error
    cbnz x0, grow_map.mremap // if we have a mapping, we have to remap
    mov  x0, x2              // set x0 to the new size
    b   alloc                // tail call into alloc. We don't need to link, just jump
grow_map.mremap:
    round_to_page x2, x2 // round x2 to the nearest multiple of 4096
    cmp  x1, x2
    b.hs grow_map.quit // we don't support shrinking
    mov  x8, #216       // mremap syscall
    mov  x3, #1         // flags = MREMAP_MAYMOVE
    svc  #0
    cmp  x0, #-1
    cset x4, eq        // set x4 to 1 if x0 == -1
    add  x0, x0, x4    // set x0 to NULL if x0 == -1 (error)
    b.eq grow_map.quit // quit now if x0 was -1
    mov  x1, x2        // set x1 to the new size
grow_map.quit:
    ret

insertsort:
    // x0 is a pointer to an array of u64
    // x1 is the end of the array
    // returns nothing
    sub  x2, x1, x0 // calculate the length of the array
    cmp  x2, #8
    b.ls insertsort.done // if the array is empty or has only one element, we're done
    add  x2, x0, #8 // move the pointer to the second element (insertion sort works like this)
insertsort.loop:  
    ldr  x4, [x2] // load current element
    mov  x3, x2   // set the inner iterator
    ldur x5, [x3, #-8] // load the previous element
    cmp  x5, x4
    b.ls insertsort.iloopend // skip loop if prev <= current
insertsort.iloop: // this loops moves all element higer than x4 one position to the right
    str  x5, [x3] // move the previous element to the current position
    sub  x3, x3, #8 // move the inner iterator to the previous element
    cmp  x3, x0 // compare the inner iterator with the start of the array
    b.ls insertsort.iloopend // loop check: if we're at the start of the array, we're done
    ldur x5, [x3, #-8] // load the (new) previous element
    cmp x5, x4         // compare the (new) previous inner element with the current external one
    b.hi insertsort.iloop // if x3 > x0 && x5 > x4, continue the loop
insertsort.iloopend: // now insert x4 back
    str x4, [x3] // store the current element in the position inner loop landed on (same if prev <= current)
    add x2, x2, #8 // move the outer iterator forward
    cmp x2, x1
    b.lo insertsort.loop // if the outer iterator is less than the end of the array, continue the loop
insertsort.done:
    ret

is_blank:
    // x0 is a character
    // returns x0 == 1 if the character is a blank, 0 otherwise
    adr x1, blanks
is_blank.loop:
    ldrb w2, [x1], #1
    cbz w2, is_blank.not_blank // no more blanks, x0 is not a blank
    cmp w0, w2
    b.ne is_blank.loop // try next one
    mov x0, #1 // x0 is a blank
    b is_blank.quit
is_blank.not_blank:
    mov x0, #0
is_blank.quit:
    ret

munmap:
    // x0 is the pointer to the memory previously mmap'd
    // x1 is the size of the chunk (should be a multiple of 4096 if allocated with alloc)
    // returns 0 on success, negative on error
    cmp  x0, #0
    ccmp x1, #0, #0b0100, ne // if x0 != 0, cmp x1. If x0 == 0, set flags to 0b0100 (zero flag set)
    b.eq munmap.quit // if !(x0 && x1), quit 
    mov  x8, #215 // munmap syscall
    svc  #0
munmap.quit:
    ret

open:
    // x0 is the path
    // returns the file descriptor in x0, or negative on error
    // note: always opens files in read-only mode (O_RDONLY)
    mov x8, #56   // openat, no open on aarch64
    mov x1, x0    // path
    mov x2, #0    // O_RDONLY
    mov x3, #0    // mode hardcoded to 0
    mov x0, #-100 // AT_FDCWD
    svc #0
    ret

parse_input:

// macro to make this more manageable, clobbers x9 and x10
.macro parse_input.fetch_slot DESTREG
    mov  x9, #24
    ldrb w10, [x29, #-32] // load the flag
    cmp  x10, #1
    csel x10, x9, xzr, eq // if odd is true, pick an offset of 24 second (buffer, cap, len)
    ldur x9, [x29, #-16] // load the struct
    add  \DESTREG, x9, x10 // add the offset
.endmacro

    // x0 is the file descriptor, x1 a struct of 6 quads
    // on error returns x0 = -1 for IO error, x0 = 0 for memory allocation error, x0 = -2 for malformed input. 1 if OK
    stp x29, lr, [sp, #-16]! // store fp and lr to the stack
    mov x29, sp // set the frame pointer to the stack pointer
    stp x1, x0, [sp, #-16]!  // `dest` (FP - 16) + `fd` (FP - 8)
    stp xzr, xzr, [sp, #-16]! // boolean `odd` (FP - 32), `next_int` (FP - 24)

parse_input.next_num:
    bl   read_u64
    cmp  x1, #0
    b.gt parse_input.done // EOF
    b.eq parse_input.read_ok
    mov x0, #-1 // IO error
    b   parse_input.fail

parse_input.read_ok:
    stur x0, [x29, #-24] // store the number in temporary variable
    parse_input.fetch_slot x4 // load the correct buffer
    ldp x0, x1, [x4] // load array and cap
    ldr x2, [x4, #16]    // load len
    cmp x2, x1
    b.lo parse_input.store_u64
    // add a 4k page, almost certainly bad. A better logic would be to multiply by 2, probably
    // ..but then I'd have to handle the 0 case, so ¯\_(ツ)_/¯
    add x2, x1, #4096
    bl  grow_map
    cbnz x0, parse_input.save_newarr
    
    mov x0, #0 // memory allocation error
    b   parse_input.fail

parse_input.save_newarr:
    parse_input.fetch_slot x4 // load the correct buffer
    stp x0, x1, [x4] // store the new array and cap
parse_input.store_u64:
    ldr  x2, [x4, #16] // load len
    ldur x3, [x29, #-24] // load next_int
    add  x0, x0, x2 // move cursor to current position
    str  x3, [x0]   // store the number
    add  x2, x2, #8 // increment the length of sizeof(u64)
    stur x2, [x4, #16] // store the new length
    ldrb w0, [x29, #-32] // load the `odd` flag
    eor x0, x0, #1 // toggle the flag
    strb w0, [x29, #-32] // store the flag
    ldur x0, [x29, #-8] // load the file descriptor
    b   parse_input.next_num // read the next chunk
parse_input.done: // successful termination
    mov x0, #1    // ok code
    b   parse_input.quit
parse_input.fail:
    stur  x0, [x29, #-24]   // store the error code in `next_int`
    ldur x4, [x29, #-16]   // load the struct
    ldp  x0, x1, [x4]      // load the array and cap #1
    bl  munmap
    ldur x4, [x29, #-16]   // load the struct
    ldp x0, x1, [x4, #24] // load the array and cap #2
    cbz x1, parse_input.fail.cleanup_done
    bl  munmap
parse_input.fail.cleanup_done:
    ldur x0, [x29, #-24] // load the error code
parse_input.quit:
    add sp, sp, #32 // discard the current frame
    ldp x29, lr, [sp], #16 // restore fp and lr
    ret

print:
    mov x2, x1
    mov x1, x0
    mov x0, #1 // stdout
    b   write // tailcall

println:
    str lr, [sp, #-16]! // store lr on the stack
    bl print // print whatever we have on x0, x1
    adr x0, newline
    mov x1, newline_len
    bl print // print the newline
    ldr lr, [sp], #16 // restore lr and sp
    ret

print_err:
    mov x2, x1
    mov x1, x0
    mov x0, #2 // stderr
    b   write // tailcall

print_fail:
    // x0 is the error message
    // x1 is the length of the error message
    bl  print_err
    mov x0, #1 // failure
    b   quit

print_usage_and_quit:
    adr x0, usage      // could use adrp + add :lo12: here but it's not worth it, we know it's going to be very close
    mov x1, usage_len
    b   print_fail

problem1:
    // x0 is the start of the first sorted list
    // x1 is the start of the second sorted list
    // x2 is the length of the two lists
    // returns the result in x0
    mov x3, x0  // use x3 as the iterator for the first list
    mov x4, x1  // use x4 as the iterator for the second list
    mov x0, xzr // use x0 as the sum
    cbz x2, problem1.done // if the lists are empty, we're done, result is 0
    add x2, x3, x2 // calculate the end of the first list
problem1.loop:
    ldr  x5, [x3], #8 // load the first number and increment the pointer
    ldr  x6, [x4], #8 // load the second number and increment the pointer
    sub  x5, x5, x6   // subtract the second number from the first
    tst  x5, x5       // check if the result is negative
    cneg x5, x5, mi   // if x5 is negative, negate it, otherwise leave it as is
    add  x0, x0, x5   // add the result to the sum
    cmp  x3, x2
    b.lo problem1.loop // if we're not at the end of the list, continue
problem1.done:
    ret

quit:
    mov x8, #93
    svc #0
    // process is dead

read:
    // x0 is the file descriptor
    // x1 is the pointer to the buffer
    // x2 is the length of the buffer
    mov x8, #63 // read syscall
    svc #0    
    ret

read_byte:
    // x0 is the file descriptor
    // returns the character in x0, x1 is 1 on EOF, or negative on error. Ok is x1 == 0 and x0 is set to the character
    stp x29, lr, [sp, #-16]! // store fp and lr to the stack
    sub sp, sp, 16 // we have 16 bytes of buffer, to store 1 char... guess it's enough
    mov x1, sp // set x1 to the buffer
    mov x2, #1 // read 1 byte
    bl  read
    cmp x0, #1
    b.eq read_byte.success
    cbnz x0, read_byte.fail // if x0 is not zero, it's an error and we have to set x1 to its value
    mov x1, #1 // EOF
    b read_byte.quit
read_byte.success:
    ldrb w0, [sp] // load the byte we just read to x0
    mov x1, xzr // clear x1
    b read_byte.quit
read_byte.fail:
    mov x1, x0 // set x1 to the error code
read_byte.quit:
    add sp, sp, #16        // restore the stack
    ldp x29, lr, [sp], #16 // restore fp and lr
    ret

// byte per byte unbuffered reading is massively inefficient, but given I'm writing AArch64 assembly I'll take what I can get
// the right way is to read a large chunk of data and then process it in memory, but it's too cumbersome for AoC purposes
read_cnk:
    // x0 is the file descriptor
    // x1 is the pointer to the buffer
    // x2 is the length of the buffer
    // sets x0 to the number of bytes read, 0 on EOF, negative on error
    cbz x2, read_cnk.end // avoid nasty underflows if the buffer is zero-length
    stp x29, lr, [sp, #-16]! // store lr and x29 on the stack
    mov x29, sp // set the frame pointer to the stack pointer
    add x2, x1, x2          // calculate the end of the buffer
    stp x1, x2, [sp, #-16]! // store the buffer and the end pointer
    stp x0, x1, [sp, #-16]! // store the file descriptor and a spare copy of the start of the buffer
    sub sp, sp, 16          // reserve 16 bytes (minimum for AArch64) for the char (8 bits) and a bool (8 bits)
    strb wzr, [sp, #1]      // This bool represents if the code is building a chunk (t) or else if it's still skipping blanks (f)
read_cnk.next_byte:
    cmp x1, x2
    b.hs read_cnk.done
    ldur x0, [x29, #-32] // load the file descriptor
    bl read_byte
    cmp x1, #0
    b.lt read_cnk.fail // if x1 is negative, it's an error
    b.gt read_cnk.done // if x1 is 1, it's EOF
    strb w0, [x29, #-48] // store the byte on the stack
    bl is_blank         // check if the byte is a blank
    cbz x0, read_cnk.store_byte // if it's not a blank, store the byte
    ldrb w1, [x29, #-47] // load the flag
    cbz w1, read_cnk.next_byte // if the flag is 0, we're still skipping blanks, so we just skip this blank
    b read_cnk.done // if the flag is 1, we've hit the end of the word, so we end the chunk
read_cnk.store_byte:
    mov  w1, #1 // set the flag to 1
    strb w1, [x29, #-47] // store the flag to true - we've got at least a non-blank character so we're appending to the buffer
    ldrb w0, [x29, #-48] // load the byte
    ldp  x1, x2, [x29, #-16] // load the buffer. Also load end so it's already there for the next iteration
    strb w0, [x1], #1   // store the byte and increment the pointer
    stur x1, [x29, #-16] // store the updated pointer
    b read_cnk.next_byte
read_cnk.done:
    ldur x1, [x29, #-16] // load the buffer
    ldur x0, [x29, #-24] // load the saved beginning of the buffer
    sub  x1, x1, x0 // calculate the number of bytes read
read_cnk.fail:
    mov x0, x1 // set x0 to the result (was x1)
    add sp, sp, #48    // restore the stack
    ldp x29, lr, [sp], #16 // restore lr and old fp
read_cnk.end:
    ret

read_u64:
    // x0 is the file descriptor
    // returns the number in x0, and 1 in x1 on EOF, 0 on success, negative on error
    stp x29, lr, [sp, #-16]! // store fp and lr to the stack
    mov x29, sp // set the frame pointer to the stack pointer
    sub sp, sp, #32 // 32 bytes for the buffer
    mov x1, sp // A u64 is max 20 chars, so we're good
    mov x2, #32
    bl read_cnk
    cmp x0, #0
    b.lt read_u64.fail // if x0 is negative, it's an error
    cset w1, eq // set w1 to 1 if EOF
    cbz  x0, read_u64.done // EOF, we read nothing, exit
    mov x1, x0 // set x1 to the length of the string
    sub x0, x29, #32 // load the buffer
    bl atoull
    mov x1, xzr // clear x1 just in case
    cbnz x0, read_u64.done // if x0 is not zero, we're good
    mov x0, #-1 // we assume that 0 is an error. May be stupid
read_u64.fail:
    mov x1, x0 // set x1 to the error code
read_u64.done:
    add sp, sp, #32    // restore the stack
    ldp x29, lr, [sp], #16 // restore lr and old fp
    ret

rewind:
    // x0 is the file descriptor
    mov x1, xzr // offset = 0
    mov x2, xzr // whence = SEEK_SET (0)
    mov x8, #62 // lseek syscall
    svc #0
    ret

ulltoa:
    // x0 is a number
    // x1 is the pointer to the buffer
    // x2 is the length of the buffer
    // returns the length of the string in x2
    cbz x2, ulltoa.exit // avoid nasty underflows if the buffer is zero-length
    add x3, x1, x2 // calculate the end of the buffer
    sub x4, x3, #1 // reverse iterator for the buffer
    strb wzr, [x4], #-1 // null-terminate the string
    mov x9, #10    // base 10
ulltoa.loop: // I know that checking loop conditions at the beginning is for n00bs, but I'm a n00b
    cmp x4, x1
    b.lo ulltoa.break // if we're at the start of the buffer, we're done with what we have
    // this incantation is the good old logic to perform integer div by a constant (10 in this case)
    mov     x5, x0                          // x5 = x0
    mov     x8, #-3689348814741910324       // =0xcccccccccccccccc
    movk    x8, #52429
    umulh   x8, x0, x8
    lsr     x0, x8, #3     // now x0 contains x0 / 10
    msub    x5, x0, x9, x5 // now x5 contains x0 % 10, aka x5 - (x5/10) * 10
    add     x5, x5, #'0'   // convert to ASCII
    strb    w5, [x4], #-1  // store the ASCII character and decrement the pointer
    cbz     x0, ulltoa.break // if x0 is zero, we're done. Note that we do this after at least one iteration
    b ulltoa.loop
ulltoa.break:
    sub x2, x3, x4 // store the string length before messing up the registers
    sub x2, x2, #1 // sub 1 for the null terminator
    // now memmove the string to the beginning of the buffer. Note that this implementation is not efficient at all
ulltoa.moveback:
    ldrb w6, [x4, #1]! // load the first character
    strb w6, [x1], #1 // store it at the beginning
    cmp x4, x3
    b.lo ulltoa.moveback
ulltoa.exit:
    ret

write:
    // x0 is the file descriptor
    // x1 is the pointer to the string
    // x2 is the length of the string
    mov x8, #64 // write syscall
    svc #0    
    ret

_start:
    mov x29, sp // set the frame pointer to the stack pointer
    // handle wrong number of arguments
    ldr  x0, [sp] // argc
    cmp  x0, #2
    b.ne print_usage_and_quit
    
    // open the file
    ldr  x0, [sp, #16] // argv[1]
    bl   open
    cmp x0, #0
    b.ge _start.file_opened

    // file open failed, print generic message because implementing printf would be too much
    adr x0, open_fail
    mov x1, open_fail_len
    bl  print_fail

_start.file_opened:
    // reserve:
    // 8 bytes for the fd (-8)
    // 8 bytes for padding (due to alignment) (-16)
    // a hefty 32 byte area for the ulltoa buffer (needs 20 anyway) (-48)
    // 48 bytes for the parse_input struct (-96)
    sub sp, sp, #96

    // zero the struct of 6 quads
    stp xzr, xzr, [sp]
    stp xzr, xzr, [sp, #16]
    stp xzr, xzr, [sp, #32]

    stur x0, [x29, #-8] // store the file descriptor on the stack
    mov  x1, sp // fp - 96
    bl   parse_input
    cmp  x0, #0
    b.hi _start.parse_ok
    add x3, x0, #2 // errors are -2, -1, 0, add 2 to get 0, 1, 2
    lsl x3, x3, #3 // multiply by 8 to get the index in the jump table
    adr x0, jump_table.parse_input.strings
    ldr x0, [x0, x3] // load the error message

    adr x1, jump_table.parse_input.strlens
    ldr x1, [x1, x3] // load the length of the error message    
    bl  print_fail    

_start.parse_ok:
    ldur x0, [x29, #-8] // load the file descriptor
    bl   close

    ldr x0, [sp] // load list #1
    ldr x1, [sp, #16] // load the length #1
    add x1, x0, x1

    bl insertsort

    ldr x0, [sp, #24] // load struct #2
    ldr x1, [sp, #40] // load the length #2
    add x1, x0, x1

    bl insertsort

    adr x0, problem1_str
    mov x1, problem1_str_len
    bl  print

    ldr x0, [sp]  // set the first list
    ldr x1, [sp, #24] // set the second list
    ldr x2, [sp, #16] // load the length of list 1 (they're the same)
    bl problem1

    sub x1, x29, #48 // ulltoa buffer
    mov x2, #32      // buffer length 
    bl  ulltoa

    sub x0, x29, #48 // ulltoa buffer
    mov x1, x2       // length of the string
    bl  println

    ldp x0, x1, [sp] // load the buffer and the capacity of the first list
    bl  munmap

    ldp x0, x1, [sp, #24] // load the buffer and the capacity of the second list
    bl  munmap

    mov x0, #0 // success    
    bl  quit

.section .rodata

alloc_fail:
    .ascii "Failed to allocate memory\n"
    .equ alloc_fail_len, . - alloc_fail

blanks:
    .asciz " \t\n\r"

malformed_input:
    .ascii "Malformed input\n"
    .equ malformed_input_len, . - malformed_input

io_fail:
    .ascii "IO error\n"
    .equ io_fail_len, . - io_fail

newline:
    .ascii "\n"
    .equ newline_len, . - newline

open_fail:
    .ascii "Failed to open file\n"
    .equ open_fail_len, . - open_fail

problem1_str:
    .ascii "Problem 1: "
    .equ problem1_str_len, . - problem1_str

problem2_str:
    .ascii "Problem 2: "
    .equ problem2_str_len, . - problem2_str

usage:
    .ascii "Usage: 01 INPUT\n"
    .equ usage_len, . - usage

jump_table.parse_input.strings:
    .quad malformed_input
    .quad io_fail
    .quad alloc_fail

jump_table.parse_input.strlens:
    .quad malformed_input_len
    .quad io_fail_len
    .quad alloc_fail_len
