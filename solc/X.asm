label_0000:
	// Inputs[1] { @0005  msg.value }
	0000    60  PUSH1 0x80
	0002    60  PUSH1 0x40
	0004    52  MSTORE
	0005    34  CALLVALUE
	0006    80  DUP1
	0007    15  ISZERO
	0008    60  PUSH1 0x0f
	000A    57  *JUMPI
	// Stack delta = +1
	// Outputs[2]
	// {
	//     @0004  memory[0x40:0x60] = 0x80
	//     @0005  stack[0] = msg.value
	// }
	// Block ends with conditional jump to 0x000f, if !msg.value

label_000B:
	// Incoming jump from 0x000A, if not !msg.value
	// Inputs[1] { @000E  memory[0x00:0x00] }
	000B    60  PUSH1 0x00
	000D    80  DUP1
	000E    FD  *REVERT
	// Stack delta = +0
	// Outputs[1] { @000E  revert(memory[0x00:0x00]); }
	// Block terminates

label_000F:
	// Incoming jump from 0x000A, if !msg.value
	// Inputs[1] { @001B  memory[0x00:0x3f] }
	000F    5B  JUMPDEST
	0010    50  POP
	0011    60  PUSH1 0x3f
	0013    80  DUP1
	0014    60  PUSH1 0x1d
	0016    60  PUSH1 0x00
	0018    39  CODECOPY
	0019    60  PUSH1 0x00
	001B    F3  *RETURN
	// Stack delta = -1
	// Outputs[2]
	// {
	//     @0018  memory[0x00:0x3f] = code[0x1d:0x5c]
	//     @001B  return memory[0x00:0x3f];
	// }
	// Block terminates

	001C    FE    *ASSERT
	// Begin Runtime
	// M[0x40] := 0x80
	001D    60    PUSH1 0x80
	001F    60    PUSH1 0x40
	0021    52    MSTORE

	0022    60    PUSH1 0x00
	0024    80    DUP1
	0025    FD    *REVERT
	0026    FE    *ASSERT
	0027    A2    LOG2
	0028    64    PUSH5 0x6970667358
	002E    22    22
	002F    12    SLT
	0030    20    SHA3
	0031    B6    B6
	0032    3E    RETURNDATACOPY
	0033    35    CALLDATALOAD
	0034    A7    A7
	0035    86    DUP7
	0036    98    SWAP9
	0037    A1    LOG1
	0038    43    NUMBER
	0039    8A    DUP11
	003A    B7    B7
	003B    AA    AA
	003C    E9    E9
	003D    39    CODECOPY
	003E    A1    LOG1
	003F    FA    STATICCALL
	0040    16    AND
	0041    63    PUSH4 0x4981ae5f
	0046    18    XOR
	0047    CF    CF
	0048    18    XOR
	0049    91    SWAP2
	004A    EE    EE
	004B    25    25
	004C    FB    FB
	004D    A1    LOG1
	004E    13    SGT
	004F    B9    B9
	0050    16    AND
	0051    64    PUSH5 0x736f6c6343
	0057    00    *STOP
	0058    08    ADDMOD
	0059    02    MUL
	005A    00    *STOP
	005B    33    CALLER
