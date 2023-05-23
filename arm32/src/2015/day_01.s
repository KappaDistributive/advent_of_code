.section .rodata

.global input

msg_1: .asciz "The answer to part 1 is: %d\n"
.balign 8
.text
.global main

output:
  mov r1, r0
  ldr r0, msg_1_addr
  bl printf
  b exit


part_1:
  mov r0, #0
  ldr r1, input_addr
part_1_loop:
  ldrb r2, [r1]
  cmp r2, #0
  beq part_1_end
  cmp r2, #'('
  addeq r0, #1
  subne r0, #1
  add r1, #1
  b part_1_loop

part_1_end:
  bx lr
  
main:
  push {lr}

  bl part_1
  mov r1, r0
  ldr r0, msg_1_addr
  bl printf
  pop {lr}
  bx lr

input_addr: .word input
msg_1_addr: .word msg_1

.global printf
