; $1: base address
; $2: size of array
; $3: 4
; $4: 4 * size
; $5: current position
; $6: char before A
; $7: ASCII for space
; $8: end label
; $9: standard output address
; $10: current address of output
; $11: value of output
; $12: ASCII number
; output words
bne $2, $0, goon
jr $31

goon:
lis $3
.word 4
mult $2, $3
mflo $4
add $5, $0, $0
lis $6
.word 64
lis $7
.word 32
lis $8
.word end
lis $9
.word 0xffff000c
sw $31, -4($30)

loop:
beq $5, $4, end
;sw $31, -4($30)
add $10, $5, $1
lw $11, 0($10)
add $5, $5, $3
bne $11, $0, chara
add $12, $7, $0
bne $6, $0, space

chara:
add $12, $6, $11

space:
sw $12, 0($9)
jalr $8
lw $31, -4($30)
jr $31

end:
bne $5, $4, loop
jr $31

