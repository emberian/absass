start:
	LI R4, 0xa5a5
	LI R7, 0x8000
	SR R, R5, 5
	LI PC, .wait

	.wait:
		SR R, R6, 5
		CMP.EGI R6, R5
		JC R6, .rotate - $ - 2
		LI PC, .again

	.rotate:
		MOV R8, R4
		BIT R8, R7, 8
		CMP.E R8, R7
		LI R9, 1
		SHL R4, R9
		JC R8, .highset - $ - 2
		LI PC, .again
	.highset:
		BIT R4, R9, 14

	.again:
		SR R, R5, 5
		LI PC, .wait
