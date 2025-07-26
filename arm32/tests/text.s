.section .rodata
msg_success: .asciz "Test `%s`: ✓\n"
msg_failure: .asciz "Test `%s`: ✘\n"

msg_bla: .asciz "bla"

name_my_strlen_empty: .asciz "my_strlen_empty"
data_my_strlen_empty: .asciz ""

name_my_strlen_simple: .asciz "my_strlen_simple"
data_my_strlen_simple: .asciz "ab cd"

name_my_strlen_simple2: .asciz "my_strlen_simple2"
data_my_strlen_simple2: .asciz "ab cd\n"

name_my_parse_next_number_empty: .asciz "my_parse_next_number_empty"
data_my_parse_next_number_empty: .asciz ""

name_my_parse_next_number_simple: .asciz "my_parse_next_number_simple"
data_my_parse_next_number_simple: .asciz "123"

name_my_parse_next_number_simple2: .asciz "my_parse_next_number_simple2"
data_my_parse_next_number_simple2: .asciz "123:"

name_my_parse_next_number_simple3: .asciz "my_parse_next_number_simple3"
data_my_parse_next_number_simple3: .asciz " 123:"

name_my_parse_next_number_simple4: .asciz "my_parse_next_number_simple4"
data_my_parse_next_number_simple4: .asciz "123:456"

.balign 8
.text
.global main

test_my_strlen:
    push {r4-r8,  lr }
    mov r5, r0
    mov r6, r1
    mov r7, r2

    mov r0, r1
    bl my_strlen

    cmp r0, r7
    movne r4, #1
    ldr r0, =msg_success
    ldrne r0, =msg_failure

    mov r1, r5
    bl printf
    
    mov r0, r4
    pop {r4-r8, lr}
    bx lr


test_my_parse_next_number:
    push {r4-r8, lr}
    mov r4, #0
    mov r5, r0
    mov r6, r2
    mov r8, r3
    sub sp, #8
    mov r0, sp
    bl my_parse_next_number
    ldr r2, [sp]
    ldr r1, [sp, #4]
    add sp, #8

    ldr r0, =msg_success

    cmp r2, r6
    movne r4, #1
    ldrne r0, =msg_failure
    
    cmp r1, r8
    movne r4, #1
    ldrne r0, =msg_failure

    mov r1, r5
    bl printf

    mov r0, r4
    pop {r4-r8, lr}
    bx lr


main:
    push {r4, lr}
    mov r4, #0 // r4 will hold the error code of all tests

    ldr r0, =name_my_strlen_empty
    ldr r1, =data_my_strlen_empty
    mov r2, #0
    bl test_my_strlen
    orr r4, r0

    ldr r0, =name_my_strlen_simple
    ldr r1, =data_my_strlen_simple
    mov r2, #5
    bl test_my_strlen
    orr r4, r0

    ldr r0, =name_my_strlen_simple2
    ldr r1, =data_my_strlen_simple2
    mov r2, #6
    bl test_my_strlen
    orr r4, r0

    ldr r0, =name_my_parse_next_number_empty 
    ldr r1, =data_my_parse_next_number_empty
    mov r2, #0
    ldr r3, =data_my_parse_next_number_empty
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple
    ldr r1, =data_my_parse_next_number_simple
    mov r2, #123
    ldr r3, =data_my_parse_next_number_simple
    add r3, #3
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple2
    ldr r1, =data_my_parse_next_number_simple2
    mov r2, #123
    ldr r3, =data_my_parse_next_number_simple2
    add r3, #3
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple3
    ldr r1, =data_my_parse_next_number_simple3
    mov r2, #0
    ldr r3, =data_my_parse_next_number_simple3
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple4
    ldr r1, =data_my_parse_next_number_simple4
    mov r2, #123
    ldr r3, =data_my_parse_next_number_simple4
    add r3, #3
    bl test_my_parse_next_number
    orr r4, r0

    mov r0, r4
    pop {r4, lr}
    bx lr

.global my_parse_next_number
.global printf
