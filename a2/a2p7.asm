; $1: base address
; $2: size of array
; $3: value 4
; $4: size * 4
; $5: counter
; $6: print procedure
; $7: copy of $1, then we can process it 

bne $0, $2, goon
jr $31

goon:
lis $3
.word 4
mult $2, $3
mflo $4
add $5, $0, $0
sw $31, -4($30)
lis $31
.word -4
add $30, $30, $31
lis $6
.word print
add $7, $0, $1

loop:
lw $1, 0($7) ; given each value
add $7, $7, $3
jalr $6
add $5, $5, $3

bne $5, $4, loop
lis $31
.word 4
add $30, $30, $31
lw $31, -4($30)
jr $31
