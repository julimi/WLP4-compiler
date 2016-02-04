; $1: given value
; $2: standard output address
; $3: newline / value 10
; $4: '0'
; $5: '-'
; $6: the index shows if the given value is negative
; $7: copy of $1 / the division results
; $8: the mod result
; $9: number (0-9) in ASCII
; $10: counter
; $11: print part
; $12: value 1
; $13: value 4

lis $2
.word 0xffff000c
lis $3
.word 10
lis $4
.word 48
lis $5
.word 45
add $7, $0, $1
add $10, $0, $0
lis $11
.word print
lis $12
.word 1
lis $13
.word 4
slt $6, $1, $0
beq $0, $6, dandm
sw $5, 0($2)
sub $7, $0, $7

; divide and modulo
dandm:
div $7, $3
mflo $7
mfhi $8
slt $6, $8, $0
beq $6, $0, process
sub $8, $0, $8

process:
add $9, $4, $8
sub $30, $30, $13
sw $9, 0($30)
add $10, $10, $12
bne $7, $0, dandm

print:
beq $10, $0, end
lw $9, 0($30)
add $30, $30, $13
sub $10, $10, $12
sw $9, 0($2)
jr $11

end:
sw $3, 0($2)
jr $31
