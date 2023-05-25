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

// input: r0=a, r1=b, r2=c
// output: total amount of wrapping paper required for aXbXc
wrapping_paper:
  push {r1-r6}
  mul r4, r0, r1  // stores total area
  mov r3, r4  // smallest area

  mul r5, r0, r2
  add r4, r5
  cmp r3, r5
  movge r3, r5

  mul r5, r1, r2
  add r4, r5
  cmp r3, r5
  movge r3, r5

  add r4, r4
  add r4, r3
  mov r0, r4

  pop {r1-r6}
  bx lr


main:
  push {lr}
  mov r0, #2
  mov r1, #3
  mov r2, #4
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
