# Constructor part -------------------------
# Stack: []
0x4	MSTORE(0x40, 0x80)
0x5	PUSH(CALLVALUE())
0x6	DUP1
0xA	JUMPI(:label0, !POP(@0x5))

# Stack: [@0x5]
0xB	PUSH(0x0)
0xD	DUP1
0xE	REVERT()

:label0
# Stack: [@0x5]
0x10	POP()
0x11	PUSH(0x99)
0x13	DUP1
0x19	CODECOPY(0x0, 0x1E, POP(0x99))
0x1C	RETURN(0x0, POP(0x99))

# Code part -------------------------
# Stack: []
0x4	MSTORE(0x40, 0x80)
0x5	PUSH(CALLVALUE())
0x6	DUP1
0xA	JUMPI(:label0, !POP(@0x5))

# Stack: [@0x5]
0xB	PUSH(0x0)
0xD	DUP1
0xE	REVERT()

:label0
# Stack: [@0x5]
0x10	POP()
0x17	JUMPI(:label1, CALLDATASIZE() < 0x4)

# Stack: []
0x1A	PUSH(CALLDATALOAD(0x0))
0x1B	PUSH(0xE0)
0x1D	Missing opcode 0x1c()
0x1E	DUP1
0x27	JUMPI(:label2, 0x371303C0 == POP(0xE0))

# Stack: [0xE0 @0x1A]
0x28	DUP1
0x31	JUMPI(:label4, 0xE1C7392A == POP(0xE0))

:label1
# Stack: []
0x33	PUSH(0x0)
0x35	DUP1
0x36	REVERT()

:label2
# Stack: [0xE0 @0x1A]
0x38	PUSH(:label3)
0x3C	JUMP(:label6)

:label3
# Stack: [0xE0 @0x1A]
0x3E	STOP()

:label4
# Stack: [0xE0 @0x1A]
0x40	PUSH(:label5)
0x44	JUMP(:label7)

:label5
# Stack: [0xE0 @0x1A]
0x46	STOP()

:label6
# Stack: [:label3 0xE0 @0x1A]
0x48	PUSH(0x0)
0x4A	DUP1
0x4B	DUP2
0x4C	PUSH(SLOAD(POP(0x0)))
0x4D	DUP1
0x4E	SWAP3
0x4F	SWAP2
0x50	SWAP1
0x53	PUSH(0x1 + POP(@0x4C))
0x54	SWAP2
0x55	SWAP1
0x56	POP()
0x57	SSTORE(POP(0x0), POP(@0x53))
0x58	POP()
0x59	JUMP(POP(:label3))

:label7
# Stack: [:label5 0xE0 @0x1A]
0x5B	PUSH(0x0)
0x5D	DUP1
0x5E	DUP2
0x5F	SWAP1
0x60	SSTORE(POP(0x0), POP(0x0))
0x61	POP()
0x62	JUMP(POP(:label5))


