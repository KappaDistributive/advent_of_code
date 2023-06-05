
.section .rodata

part_1_msg: .asciz "The answer to part one is: %d\n"
part_2_msg: .asciz "The answer to part two is: %d\n"

.balign 8
.text
.global my_strlen
.global main

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


part_2:
    push {r0, r4-r6, lr}
    
    push {r0}
    bl my_strlen
    mov r4, r0  // r4 = length of input string
    pop {r0}

    mov r3, #0  // r3 = answer
    mov r5, #0  // r5 = offset
    
part_2_loop:
    add r1, r0, r5
    ldrb r1, [r1]
    cmp r1, #0
    beq part_2_end

    add r6, r5, r4, ASR #1
    cmp r6, r4
    subge r6, r6, r4
part_2_test:
    add r6, r0
    ldrb r2, [r6]

    cmp r1, r2
    addeq r3, r1
    subeq r3, #'0'
    add r5, #1
    b part_2_loop

part_2_end:
    pop {r0, r4-r6, lr}
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

    push {r0}
    bl part_2
    mov r1, r0
    ldr r0, =part_2_msg
    bl printf
    pop {r0}


    mov r0, #0
    pop {lr}
    bx lr

.global printf
