ADD PC,$start
SUB PC,$start
start:
BIT R2, R2, 9
LI R3, 1
SR W, R3, 3
XF PC, *PC+
.WORD forward;
again: MOV R1, R2
LI R1, 0xabcd
forward:
LI PC, .two
.two:
LI PC, again

AND R3, R3
OR R4, R4
XOR R5, R5
NOT R6
NAND R7, R7
NOR R8, R8
XNOR R9, R9

ZERO PC
ZERO SP
ZERO FP
ZERO RA
ONE A0
ONE A5
ONE T0
ONE T5
