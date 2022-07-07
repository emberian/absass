BIT R2, R2, 9
LI R3, 1
SR W, R3, 3
XF PC, *PC+
.WORD forward
again: MOV R1, R2
LI R1, 0xabcd
forward:
LI PC, .two
.two:
LI PC, again

