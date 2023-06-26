.text
.global my_strlen
.global my_parse_next_number

my_parse_next_number:
/*
input:
    r0 = address of 8 byte output buffer.
    r1 = address to first byte of null-terminated string.

return:
    [r0] = value of next number; 0 if none was found.
    [r0, #4] = address of first byte after next number in input.

remarks:
    - Only supports non-negative integers < 2**32.
*/
    push {r4}
    mov r4, #10
    mov r3, #0
my_parse_next_number_loop:
    ldrb r2, [r1]
    cmp r2, #'0'
    blt my_parse_next_number_end
    cmp r2, #'9'
    bgt my_parse_next_number
    mul r3, r3, r4
    add r3, r2
    sub r3, #'0'
    add r1, #1
    b my_parse_next_number_loop

my_parse_next_number_end:
    str r3, [r0]
    str r1, [r0, #4]
    pop {r4}
    bx lr

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