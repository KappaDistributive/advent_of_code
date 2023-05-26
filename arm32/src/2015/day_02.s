.section .rodata

.global input

msg_1: .asciz "The answer to part 1 is: %d\n"
msg_2: .asciz "The answer to part 2 is: %d\n"
msg_debug: .asciz "%d\t%d\t%d\n"

.balign 8
.data

triple: .skip 3*4

.balign 8
.text
.global main

wrapping_paper:
/*
  input: r0=a, r1=b, r2=c
  output: total amount of wrapping paper required for aXbXc
*/
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

parse_line: 
/*
  input:
    - r0: address of the desired output. Needs to be able to hold 4*4 bytes.
    - r1: address of first byte of the string to be parsed; of the form aXbXc(\n|\0),
      where a,b,c, are non-negative integers
  output: 4*4 bytes
    - address of the byte in the input string we stopped at
    - integer value of a
    - integer value of b
    - integer value of c
*/
  push {r0, r4, r5}
  // r1: the position of the current character we are parsing
  // r2: the current character we are parsing
  // r3: offset for storage
  // r4: partially parsed value
  // r5: is always 10
  mov r3, #4
  mov r4, #0
  mov r5, #10

parse_line_loop:
  ldrb r2, [r1]
  
  // check whether we are at the end of a number
  cmp r2, #'x'
  beq parse_line_store
  cmp r2, #'\n'
  beq parse_line_store
  cmp r2, #0
  beq parse_line_store

  // update
  mul r4, r5
  sub r2, #'0'
  add r4, r2
  add r1, #1
  b parse_line_loop

parse_line_store:
  push {r0}
  add r0, r3
  str r4, [r0]
  pop {r0}
  cmp r3, #12
  beq parse_line_end
  mov r4, #0
  add r1, #1
  add r3, #4
  b parse_line_loop

parse_line_end:
  pop {r0, r4, r5}
  bx lr


main:
  push {lr}
  sub sp, #16
  
  mov r0, sp
  ldr r1, =input_addr
  ldr r1, [r1]
  bl parse_line

  ldr r1, [sp, #4]
  ldr r2, [sp, #8]
  ldr r3, [sp, #12]

  ldr r0, msg_debug_addr
  bl printf
  
  // bl wrapping_paper

  // mov r1, r0

  // ldr r0, msg_1_addr
  // bl printf
 
  add sp, #16
  pop {lr}
  bx lr

input_addr: .word input
msg_1_addr: .word msg_1
msg_2_addr: .word msg_2
msg_debug_addr: .word msg_debug
triple_addr: .word triple

.global printf
