.section .rodata
msg_success: .asciz "Test `%s`: ✓\n"
msg_failure: .asciz "Test `%s`: ✘\n"

name_my_parse_next_number_empty: .asciz "my_parse_next_number_empty"
data_my_parse_next_number_empty: .asciz ""

name_my_parse_next_number_simple: .asciz "my_parse_next_number_simple"
data_my_parse_next_number_simple: .asciz "123"

name_my_parse_next_number_simple2: .asciz "my_parse_next_number_simple2"
data_my_parse_next_number_simple2: .asciz "123:"

.balign 8
.text
.global main

test_my_parse_next_number:
    push {r4-r6, lr}
    mov r4, #0
    mov r5, r0
    mov r6, r2
    sub sp, #8
    mov r0, sp
    bl my_parse_next_number
    ldr r0, [sp]
    add sp, #8
    cmp r0, r6
    ldreq r0, =msg_success
    movne r4, #1
    ldrne r0, =msg_failure
    
    mov r1, r5
    bl printf

    mov r0, r4
    pop {r4-r6, lr}
    bx lr


main:
    push {r4, lr}
    mov r4, #0

    ldr r0, =name_my_parse_next_number_empty 
    ldr r1, =data_my_parse_next_number_empty
    mov r2, #0
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple
    ldr r1, =data_my_parse_next_number_simple
    mov r2, #123
    bl test_my_parse_next_number
    orr r4, r0

    ldr r0, =name_my_parse_next_number_simple2
    ldr r1, =data_my_parse_next_number_simple2
    mov r2, #123
    bl test_my_parse_next_number
    orr r4, r0

    mov r0, r4
    pop {r4, lr}
    bx lr

.global my_parse_next_number
.global printf
