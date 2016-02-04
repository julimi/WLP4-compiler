;$1:input value
;$3:current value
;$4: value 4
;$5: result value from slt
;$6: hi value of division
;$7: lo value of division
;$8: ASCII of 0
;$9: count the time of loop;
;$10: value 10
;$11: value 1
;$12: ASCII for new line
;$15: loop address
;$20:ASCII code for the ouput char
;$21;address of the std output
print:
	sw $1,-4($30);
        sw $3,-8($30);
        sw $4,-12($30);
        sw $5,-16($30);
        sw $6,-20($30);
        sw $7,-24($30);
        sw $8,-28($30);
        sw $9,-32($30);
        sw $10,-36($30);
        sw $11,-40($30);
        sw $12,-44($30);
        sw $15,-48($30);
        sw $20,-52($30);
        sw $21,-56($30);
	sw $22,-60($30);
	lis $22;
	.word 60;
	sub $30,$30,$22;

	lis $12;
	.word 10;
	lis $4;
	.word 4;
	lis $11;
	.word 1;
	lis $10;
	.word 10;
        lis $8;
        .word 48;ASCII of 0 to $8        
	lis $21;
        .word 0xffff000c;
	add $9,$0,$0;
	add $3,$1,$0;copy $1 to $3
	slt $5,$3,$0;check if input value is negative
	beq $5,$0,start;skip to start if the number is positive
	lis $20; 
	.word 45;store ASCII of - to $20
	sw $20,0($21);output -
start:	
	div $3,$10;
	mflo $3;the result of division to $3
	mfhi $6;the mod result 
	slt $5,$6,$0;check if $6 is positive
	beq $5,$0,skip;
	sub $6,$0,$6;if $6 is negative,negate it 
skip:
	add $20,$6,$8;
	sub $30,$30,$4;
	sw $20,0($30);push the value to stack
	add $9,$9,$11;
	bne $3,$0,start;
	lis $15;
	.word loop;
loop:
	beq $0,$9,end;return if $9 is zero
	lw $20,0($30);pop the value
	add $30,$30,$4;
	sw $20,0($21);
	sub $9,$9,$11;
	jr $15;
end:
        add $30,$30,$22;
	sw $12,0($21);
        lw $1,-4($30);
        lw $3,-8($30);
        lw $4,-12($30);
        lw $5,-16($30);
        lw $6,-20($30);
        lw $7,-24($30);
        lw $8,-28($30);
        lw $9,-32($30);
        lw $10,-36($30);
        lw $11,-40($30);
        lw $12,-44($30);
        lw $15,-48($30);
        lw $20,-52($30);
        lw $21,-56($30);
        lw $22,-60($30);
	jr $31;
