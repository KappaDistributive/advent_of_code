.section .rodata

.global input

msg_1: .asciz "The answer to part 1 is: %d\n"
msg_2: .asciz "The answer to part 2 is: %d\n"

.balign 8
.text
.global main

ribbon:
 /*
  input: r0=a, r1=b, r2=c
  output: r0=total length of ribbon for aXbXc
*/
  push {r4-r6}

  // volume
  mul r4, r0, r1
  mul r4, r4, r2

  // perimeter of smallest size
  add r5, r0, r1
  mov r5, r5, LSL #1

  add r6, r0, r2
  mov r6, r6, LSL #1
  cmp r6, r5
  movle r5, r6
  
  add r6, r1, r2
  mov r6, r6, LSL #1
  cmp r6, r5
  movle r5, r6
  
  add r0, r4, r5
  pop {r4-r6}
  bx lr

wrapping_paper:
/*
  input: r0=a, r1=b, r2=c
  output: r0=total amount of wrapping paper required for aXbXc
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
  str r1, [r0]
  bx lr

main:
  push {r4-r5, lr}
  sub sp, #16
  mov r4, #0 // collects answer for part one
  mov r5, #0 // collects answer for part one


  ldr r1, =input_addr
  ldr r1, [r1]
  sub r1, #1
main_loop:
  add r1, #1
  mov r0, sp
  bl parse_line

  ldr r0, [sp, #4]
  ldr r1, [sp, #8]
  ldr r2, [sp, #12]
  bl wrapping_paper
  add r4, r0

  ldr r0, [sp, #4]
  ldr r1, [sp, #8]
  ldr r2, [sp, #12]
  bl ribbon
  add r5, r0

  
  ldr r1, [sp]
  ldrb r2, [r1]
  cmp r2, #0
  bne main_loop
  
  ldr r0, msg_1_addr
  mov r1, r4
  bl printf

  ldr r0, msg_2_addr
  mov r1, r5
  bl printf

  add sp, #16
  pop {r4-r5, lr}
  bx lr

input_addr: .word input
msg_1_addr: .word msg_1
msg_2_addr: .word msg_2

.global printf
