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

test_my_parse_next_number_empty:
    push {r4, lr}
    mov r4, #0
    sub sp, #8
    mov r0, sp
    ldr r1, =data_my_parse_next_number_empty
    bl my_parse_next_number
    ldr r0, [sp]
    add sp, #8
    cmp r0, #0
    ldreq r0, =msg_success
    movne r4, #1
    ldrne r0, =msg_failure
    
    ldr r1, =name_my_parse_next_number_empty
    bl printf

    mov r0, r4
    pop {r4, lr}
    bx lr


test_my_parse_next_number_simple:
    push {r4, lr}
    mov r4, #0
    sub sp, #8
    mov r0, sp
    ldr r1, =data_my_parse_next_number_simple
    bl my_parse_next_number
    ldr r0, [sp]
    add sp, #8
    cmp r0, #123
    ldreq r0, =msg_success
    movne r4, #1
    ldrne r0, =msg_failure
    
    ldr r1, =name_my_parse_next_number_simple
    bl printf

    mov r0, r4
    pop {r4, lr}
    bx lr


test_my_parse_next_number_simple2:
    push {r4, lr}
    mov r4, #0
    sub sp, #8
    mov r0, sp
    ldr r1, =data_my_parse_next_number_simple2
    bl my_parse_next_number
    ldr r0, [sp]
    add sp, #8
    cmp r0, #123
    ldreq r0, =msg_success
    movne r4, #1
    ldrne r0, =msg_failure
    
    ldr r1, =name_my_parse_next_number_simple2
    bl printf

    mov r0, r4
    pop {r4, lr}
    bx lr


main:
    push {r4, lr}
    mov r4, #0

    bl test_my_parse_next_number_empty
    orr r4, r0

    bl test_my_parse_next_number_simple
    orr r4, r0

    bl test_my_parse_next_number_simple2
    orr r4, r0

    mov r0, r4
    pop {r4, lr}
    bx lr

.global my_parse_next_number
.global printf
