; $1: base address
; $2: size of array
; $3: the maximum of all elements 
; Assume the array is not empty
lis $4
.word 4
mult $2, $4
mflo $5
lw $3, 0($1)
add $6, $0, $0

loop:
add $7, $1, $6
add $6, $4, $6
lw $8, 4($7)
slt $9, $3, $8
beq $9, $0, end
add $3, $0, $8  
end:
bne $5, $6, loop
jr $31
