; $1: the bas address
; $2: the size of array
; $3: -array is empty, -1
;     -otherwise, the last element

beq $2, $0, empty
lis $4
.word 4
mult $2, $4
mflo $4
add $4, $4, $1
lw $3, -4($4)
jr $31

empty:
lis $3
.word -1
jr $31
