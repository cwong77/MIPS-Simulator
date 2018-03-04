
	.text

main:
	li $a0, 0
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall
	
	li $a0, 1
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall
	
	li $a0, 196
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall
	
	li $a0, -1
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall
	
	li $a0, -452
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall
	
	li $a0, 7
	jal mystery
	move $a0, $v0
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall

	li $a0, 32
	jal mystery
	move $a0, $v0
	jal putDec
	li $a0, '\n'
	li $v0, 11
	syscall

	li 	$v0, 10		# terminate program
	syscall

putDec: 
	## FILL IN ##
	beq	$a0, $0, zero
	blt	$a0, 0, negative
	li	$t0, 10
loop:
	addi	$sp, $sp, -8
	sw	$ra, 0($sp)
	divu	$a0, $t0	#divde by 10
	mflo	$a0		#save the quotient into a0
	mfhi	$t1		#save remainder into t1
	sw	$t1, 4($sp)
	beq	$a0, $zero, print
	jal 	loop
print:
	lw	$a0, 4($sp)
	addi	$a0, $a0, 48
	li	$v0, 11
	syscall
	lw	$ra, 0($sp)
	add	$sp, $sp, 8
	jr	$ra
	
zero:
	addi	$a0, $a0, 48
	li	$v0, 11
	syscall
	jr $ra
	
negative:
	mul	$t2, $a0, -1
	li	$a0, '-'
	li	$v0, 11
	syscall
	move	$a0, $t2
	j 	loop

mystery: bne $0, $a0, recur 	# 
 	li $v0, 0 		#
 	jr $ra 			#
 recur: sub $sp, $sp, 8 	#
 	sw $ra, 4($sp) 	#
 	sub $a0, $a0, 1 	#
 	jal mystery 		#
 	sw $v0, 0($sp) 	#
 	jal mystery 		#
 	lw $t0, 0($sp) 	#
 	addu $v0, $v0, $t0 	#
 	addu $v0, $v0, 1 	#
 	add $a0, $a0, 1 	#
 	lw $ra, 4($sp) 	#
 	add $sp, $sp, 8 	#
 	jr $ra 			#
