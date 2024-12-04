.text
.globl _start

atoull:
    // x0 is the pointer to the string
    // x1 is the length of the string
    // returns the number in x0, or 0 on error
    add x2, x0, x1 // calculate the end of the string
    mov x1, x0     // use x1 as iterator
    mov x0, xzr    // clear x0
    mov x4, #10    // base 10
atoull.loop:
    cmp x1, x2
    b.ge atoull.fail // if we're at the end of the string, we're done
    ldrb w3, [x1], #1 // load the byte and increment the pointer
    sub w3, w3, #'0'  // check if it's a digit
    cmp w3, 10
    b.cs atoull.fail  // if the char is between ['0', '10'], (w3 - '0') < 10. Note, unsigned flag (carry set)
    madd x0, x0, x4, x3 // x0 = x0 * 10 + x3
    b atoull.loop
atoull.fail:
    ret

close:
    // expect file descriptor in x0
    mov x8, #57 // close syscall
    svc #0
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

open:
    mov x8, #56   // openat, no open on aarch64
    mov x1, x0    // path
    mov x2, #0    // O_RDONLY
    mov x3, #0    // mode hardcoded to 0
    mov x0, #-100 // AT_FDCWD
    svc #0
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

// byte per byte unbuffered reading is super inefficient, but given I'm writing AArch64 assembly I'll take what I can get
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
    b.ge read_cnk.done
    ldr x0, [x29, #-32] // load the file descriptor
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
    mov w1, #1 // set the flag to 1
    strb w1, [x29, #-47] // store the flag to true - we've got at least a non-blank character so we're appending to the buffer
    ldrb w0, [x29, #-48] // load the byte
    ldp x1, x2, [x29, #-16] // load the buffer. Also load end so it's already there for the next iteration
    strb w0, [x1], #1   // store the byte and increment the pointer
    str x1, [x29, #-16] // store the updated pointer
    b read_cnk.next_byte
read_cnk.done:
    ldr x1, [x29, #-16] // load the buffer
    ldr x0, [x29, #-24] // load the saved beginning of the buffer
    sub x1, x1, x0 // calculate the number of bytes read
read_cnk.fail:
    mov x0, x1 // set x0 to the result (was x1)
    add sp, sp, #48    // restore the stack
    ldp x29, lr, [sp], #16 // restore lr and old fp
read_cnk.end:
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
ulltoa.loop:
    cmp x4, x1
    b.lt ulltoa.break // if we're at the start of the buffer, we're done with what we have
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
    // now memmove the string to the beginning of the buffer. Note that this implementation is not efficient at all
ulltoa.moveback:
    ldrb w6, [x4], #1 // load the first character
    strb w6, [x1], #1 // store it at the beginning
    cmp x4, x3
    b.lt ulltoa.moveback
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
    str x0, [sp, #-16]! // store the file descriptor on the stack (and realign sp)
    mov x29, sp // set the frame pointer to the stack pointer
    sub sp, sp, #544 // reserve 512 bytes for the buffer + 32 bytes for shenanigans
_start.read_chunk:
    ldr  x0, [x29] // load the file descriptor
    sub  x1, x29, 512 // set x1 to the buffer
    mov  x2, #512 // set x2 to the length of the buffer
    bl   read_cnk
    cmp  x0, #0
    b.le _start.done

    mov x1, x0 // the length of the chunk 
    sub x0, x29, #512 // the buffer 
    bl  atoull

    mov x1, sp // the buffer for the bad stuff
    mov x2, #32 // the length of the buffer
    bl  ulltoa 

    mov x0, sp
    mov x1, x2
    bl  println

    b   _start.read_chunk // read the next chunk

_start.done:
    str x0, [sp, #-16]! // store the return value on the stack 

    ldr x0, [x29] // the file descriptor
    bl  close
    
    ldr x0, [sp], #16 // restore the return value
    cbz x0, _start.quit

    // print we had an IO error and die
    adr x0, io_fail
    mov x1, io_fail_len
    bl  print_fail

_start.quit:
    bl  quit

.section .rodata

blanks:
    .asciz " \t\n\r"

newline:
    .ascii "\n"
    .equ newline_len, . - newline

usage:
    .ascii "Usage: 01 INPUT\n"
    .equ usage_len, . - usage

open_fail:
    .ascii "Failed to open file\n"
    .equ open_fail_len, . - open_fail

io_fail:
    .ascii "IO error\n"
    .equ io_fail_len, . - io_fail
