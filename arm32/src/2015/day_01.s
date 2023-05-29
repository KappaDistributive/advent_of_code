.section .rodata

msg_1: .asciz "The answer to part 1 is: %d\n"
msg_2: .asciz "The answer to part 2 is: %d\n"

.balign 8
.text
.global main

output:
  mov r1, r0
  ldr r0, msg_1_addr
  bl printf
  b exit


part_1:
  mov r1, r0
  mov r0, #0
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

part_2:
  mov r1, r0
  mov r0, #0
  mov r3, #0
part_2_loop:
  ldrb r2, [r1]
  cmp r0, #-1
  beq part_2_end
  cmp r2, #0
  beq part_2_err
  add r3, #1
  cmp r2, #'('
  addeq r0, #1
  subne r0, #1
  add r1, #1
  b part_2_loop

part_2_err:
  mov r0, #-1
  bx lr

part_2_end:
  mov r0, r3
  bx lr
  
main:
  push {lr}
  ldr r0, [r1, #4]
  push {r0}

  ldr r0, [sp]
  bl part_1
  mov r1, r0
  ldr r0, msg_1_addr
  bl printf

  ldr r0, [sp]
  bl part_2
  mov r1, r0
  ldr r0, msg_2_addr
  bl printf

  pop {r0, lr}
  mov r0, #0
  bx lr

msg_1_addr: .word msg_1
msg_2_addr: .word msg_2


.global printf
