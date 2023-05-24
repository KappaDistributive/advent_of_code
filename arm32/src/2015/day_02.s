.section .rodata

.global input

msg_1: .asciz "The answer to part 1 is: %d\n"
msg_2: .asciz "The answer to part 2 is: %d\n"

.balign 8
.data

triple: .skip 3*4

.balign 8
.text
.global main

// input: address of triple (a, b, c) of 32 bit integers in r0
// output: total amount of wrapping paper required for aXbXc
wrapping_paper:
  push {r1-r6}
  mov r6, #0    // smallest side
  ldr r1, [r0], #4
  ldr r2, [r0], #4
  ldr r3, [r0]
  mul r4, r1, r2
  mov r6, r4

  mul r5, r2, r3
  add r4, r5
  cmp r6, r5
  movge r6, r5

  mul r5, r1, r3
  add r4, r5
  cmp r6, r5
  movge r6, r5

  add r4, r4
  add r4, r6
  mov r0, r4
  pop {r1-r6}
  bx lr


main:
  push {lr}
  ldr r1, =triple_addr
  ldr r1, [r1]
  mov r0, #2
  str r0, [r1], #4
  mov r0, #3
  str r0, [r1], #4
  mov r0, #4
  str r0, [r1]

  ldr r0, =triple_addr
  ldr r0, [r0]
  bl wrapping_paper

  mov r1, r0

  ldr r0, msg_1_addr
  bl printf
  
  pop {lr}
  bx lr

input_addr: .word input
msg_1_addr: .word msg_1
msg_2_addr: .word msg_2
triple_addr: .word triple

.global printf
