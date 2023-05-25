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
