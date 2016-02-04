; compare $1 < $2
slt $3, $1, $2
; if false, skip the next instr
beq $3, $0, 2
add $3, $0, $2
jr $31
add $3, $0, $1
jr $31

