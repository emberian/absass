# For assrs (probably)
.WS 64
# For myhdl
#.WS 16
LI R4, chars

loop:
	XF R6, *R4+
	ZERO R5
	CMP.E R5, R6
	JC R5, done - $ - 2
	SR W, R6, 10
	LI PC, loop

done:
	LI R5, 1
	SR W, R5, 3

chars:
	.WORD "Hello world!", 0;
