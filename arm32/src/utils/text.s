.text
.global my_strlen

my_strlen:
/*
input:
    r0 = address to first byte of null-terminated string.
return:
    r0 = length of string (number of bytes).
*/
    push {r4}
    mov r1, r0
    mov r0, #0
    mov r3, #0xff
my_strlen_loop:
    ldr r2, [r1]

    // check least significant byte
    and r4, r2, r3
    cmp r4, #0
    beq my_strlen_end
    add r0, #1

    and r4, r2, r3, LSL #8
    cmp r4, #0
    beq my_strlen_end
    add r0, #1
    
    and r4, r2, r3, LSL #16
    cmp r4, #0
    beq my_strlen_end
    add r0, #1

    // check most significant byte
    and r4, r2, r3, LSL #24
    cmp r4, #0
    beq my_strlen_end
    add r0, #1

    add r1, #4
    b my_strlen_loop

my_strlen_end:
    pop {r4}
    bx lr
