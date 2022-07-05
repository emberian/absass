BIT R2, R2, 9
LI PC, forward
again: MOV R1, R2
LI R1, 0xabcd
forward:
LI PC, again

