lis $4
.word 1
add $5, $0, $4
add $3, $0, $4
lis $6
.word 3
lis $8
.word 2
add $9, $0, $4
lis $11
.word loop
div $2, $6
mflo $7

loop:
slt $10, $5, $7
beq $10, $0, end
mult $8, $9
mflo $9
add $5, $5, $9
add $3, $3, $4 
jr $11

end:
jr $31
