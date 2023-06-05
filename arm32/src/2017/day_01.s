
.section .rodata

part_1_msg: .asciz "The answer to part one is: %d\n"

.text
.global main

strlen:
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
strlen_loop:
    ldr r2, [r1]

    // check least significant byte
    and r4, r2, r3
    cmp r4, #0
    beq strlen_end
    add r0, #1

    and r4, r2, r3, LSL #8
    cmp r4, #0
    beq strlen_end
    add r0, #1
    
    and r4, r2, r3, LSL #16
    cmp r4, #0
    beq strlen_end
    add r0, #1

    // check most significant byte
    and r4, r2, r3, LSL #24
    cmp r4, #0
    beq strlen_end
    add r0, #1

    add r1, #4
    b strlen_loop

strlen_end:
    pop {r4}
    bx lr

part_1:
    push {r0}
    mov r3, #0
    
part_1_loop:
    ldrb r1, [r0], #1
    cmp r1, #0
    beq part_1_end
    ldrb r2, [r0]
    cmp r2, #0
    beq part_1_end
    cmp r1, r2
    addeq r3, r1
    subeq r3, #'0'
    b part_1_loop

part_1_end:
    pop {r0}
    ldrb r2, [r0]
    cmp r1, r2
    addeq r3, r1
    subeq r3, #'0'

    mov r0, r3
    bx lr

main:
    push {lr}
    ldr r0, [r1, #4]
    
    push {r0}
    bl part_1
    mov r1, r0
    ldr r0, =part_1_msg
    bl printf
    pop {r0}

    mov r0, #0
    pop {lr}
    bx lr

.global printf
