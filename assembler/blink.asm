start:
	LI R4, 0xa5a5  # Pattern to be sent to the LEDs
	LI R7, 0x8000  # High bit
	LI R9, 1       # Low bit
	SR R, R5, 5    # Read from cycles register
	LI PC, .wait

# Wait until the cycle counter rolls over
# (with 16 bits, that happens in 64kicycles)
	.wait:
		SR R, R6, 5
		CMP.EGI R6, R5  # !(R6 >= R5) -> R6 < R5
		JC R6, .rotate - $ - 2
		LI PC, .again

	.rotate:
		MOV R8, R4
		BIT R8, R7, 8  # Extract the high bit (AND)
		CMP.E R8, R7   # Are they equal?
		SHL R4, R9     # R4 <<= 1
		JC R8, .highset - $ - 2
		LI PC, .again
	.highset:
		BIT R4, R9, 14 # R4 |= 1 (OR)

	.again:
		SR R, R5, 5
		LI PC, .wait
