	.text
main:	add	$t0, $s0, $zero
	add	$t1, $s1, $zero
	add	$t2, $t1, $t0
	add	$t3, $t2, $t1
	add	$t4, $t3, $t2
	add	$t5, $t4, $t3
	add	$t6, $t5, $t4
	add	$t7, $t6, $t5
	li	$v0, 10
	syscall
