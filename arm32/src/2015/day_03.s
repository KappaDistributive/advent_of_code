.section .rodata
msg1: .asciz "The answer to part 1 is: %d\n"

.balign 8
.data
buffer: .skip 400000

.balign 8
.text
.global main

part_1:
  push {r1-r6, lr}
  ldr r6, =buffer             // r6 = current position in buffer
  ldr r1, [r1, #4]            // r1 = position of current character
  ldrb r2, [r1]               // r2 = current character

  mov r4, #0                  // current x
  mov r5, #0                  // current y
  str r4, [r6], #4
  str r5, [r6], #4

part_1_travel_loop:
  cmp r2, #0
  beq part_1_end_travel
  cmp r2, #'<'
  subeq r4, #1
  cmp r2, #'>'
  addeq r4, #1
  cmp r2, #'^'
  subeq r5, #1
  cmp r2, #'v'
  addeq r5, #1
  
  str r4, [r6], #4
  str r5, [r6], #4
  
  add r1, #1
  ldrb r2, [r1]
  b part_1_travel_loop

part_1_end_travel:
  mov r1, #0                  // number of unique positions
  ldr r2, =buffer             // current position in buffer

part_1_check_outer_loop:
  cmp r2, r6
  beq part_1_end
  ldr r3, =buffer
part_1_check_inner_loop:
  cmp r3, r2
  addeq r2, #8
  addeq r1, #1
  beq part_1_check_outer_loop
  
  push {r0, r1}
  mov r4, #0
  ldr r0, [r2]
  ldr r1, [r3]
  cmp r0, r1
  addeq r4, #1
  ldr r0, [r2, #4]
  ldr r1, [r3, #4]
  cmp r0, r1
  pop {r0, r1}
  addeq r4, #1
  // r4 == 2 if this position is not unique
  cmp r4, #2
  // move on to the next position and short-circuit the inner loop
  addeq r2, #8
  beq part_1_check_outer_loop

  add r3, #8
  b part_1_check_inner_loop

part_1_end:
  mov r0, r1
  pop {r1-r6, lr}
  bx lr

main:
  push {r1-r6, lr}

  bl part_1
  ldr r0, =msg1
  bl printf
  mov r0, #0
  pop {r1-r6, lr}
  bx lr
  
.global printf
