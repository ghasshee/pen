00000000:   PUSH1 "80"
00000002:   PUSH1 "40"
00000004:   MSTORE
00000005:   CALLVALUE
00000006:   DUP1
00000007:   ISZERO
00000008:   PUSH2 "0010"
0000000B:   JUMPI
0000000C:   PUSH1 "00"
0000000E:   DUP1
0000000F:   REVERT
00000010:   JUMPDEST "10"
00000011:   POP
00000012:   PUSH1 "BE"
00000014:   DUP1
00000015:   PUSH2 "001F"
00000018:   PUSH1 "00"
0000001A:   CODECOPY
0000001B:   PUSH1 "00"
0000001D:   RETURN
0000001E:   INVALID
// RUNTIME
00000000:   PUSH1 "80"
00000002:   PUSH1 "40"
00000004:   MSTORE
00000005:   CALLVALUE
00000006:   DUP1
00000007:   ISZERO
00000008:   PUSH1 "0F"
0000000A:   JUMPI
0000000B:   PUSH1 "00"
0000000D:   DUP1
0000000E:   REVERT
0000000F:   JUMPDEST "F"
00000010:   POP
00000011:   PUSH1 "04"
00000013:   CALLDATASIZE
00000014:   LT
00000015:   PUSH1 "32"
00000017:   JUMPI
00000018:   PUSH1 "00"
0000001A:   CALLDATALOAD
0000001B:   PUSH1 "E0"
0000001D:   SHR
0000001E:   DUP1
0000001F:   PUSH4 "371303C0"
00000024:   EQ
00000025:   PUSH1 "37"
00000027:   JUMPI
00000028:   DUP1
00000029:   PUSH4 "6D4CE63C"
0000002E:   EQ
0000002F:   PUSH1 "3F"
00000031:   JUMPI
00000032:   JUMPDEST "32"
00000033:   PUSH1 "00"
00000035:   DUP1
00000036:   REVERT
00000037:   JUMPDEST "37"
00000038:   PUSH1 "3D"
0000003A:   PUSH1 "54"
0000003C:   JUMP
0000003D:   JUMPDEST "3D"
0000003E:   STOP
0000003F:   JUMPDEST "3F"
00000040:   PUSH1 "00"
00000042:   SLOAD
00000043:   PUSH1 "40"
00000045:   MLOAD
00000046:   SWAP1
00000047:   DUP2
00000048:   MSTORE
00000049:   PUSH1 "20"
0000004B:   ADD
0000004C:   PUSH1 "40"
0000004E:   MLOAD
0000004F:   DUP1
00000050:   SWAP2
00000051:   SUB
00000052:   SWAP1
00000053:   RETURN
00000054:   JUMPDEST "54"
00000055:   PUSH1 "00"
00000057:   SLOAD
00000058:   PUSH1 "60"
0000005A:   SWAP1
0000005B:   PUSH1 "01"
0000005D:   PUSH1 "65"
0000005F:   JUMP
00000060:   JUMPDEST "60"
00000061:   PUSH1 "00"
00000063:   SSTORE
00000064:   JUMP
00000065:   JUMPDEST "65"
00000066:   PUSH1 "00"
00000068:   DUP3
00000069:   NOT
0000006A:   DUP3
0000006B:   GT
0000006C:   ISZERO
0000006D:   PUSH1 "83"
0000006F:   JUMPI
00000070:   PUSH4 "4E487B71"
00000075:   PUSH1 "E0"
00000077:   SHL
00000078:   DUP2
00000079:   MSTORE
0000007A:   PUSH1 "11"
0000007C:   PUSH1 "04"
0000007E:   MSTORE
0000007F:   PUSH1 "24"
00000081:   DUP2
00000082:   REVERT
00000083:   JUMPDEST "83"
00000084:   POP
00000085:   ADD
00000086:   SWAP1
00000087:   JUMP
00000088:   INVALID
// IPFS/VER
00000000:   INFO "A2"
00000001:   INFO "64"
00000002:   INFO "ipfs5822"
00000003:   INFO "1220EBBF9E6530745C2246C5EC9DF0F41280941AB72D344700CCF6BFF96F2DF6469C"
00000004:   INFO "64"
00000005:   INFO "solc43"
00000006:   INFO "000802"
00000007:   INFO "00"
00000008:   INFO "33"
[JUMPI,PUSH2 "0010",ISZERO,DUP1,CALLVALUE,MSTORE,PUSH1 "40",PUSH1 "80"]
[REVERT,DUP1,PUSH1 "00"]
[JUMPDEST "10"]
[RETURN,PUSH1 "00",CODECOPY,PUSH1 "00",PUSH2 "001F",DUP1,PUSH1 "BE",POP]
[INVALID]
[JUMPI,PUSH1 "0F",ISZERO,DUP1,CALLVALUE,MSTORE,PUSH1 "40",PUSH1 "80"]
[REVERT,DUP1,PUSH1 "00"]
[JUMPDEST "F"]
[JUMPI,PUSH1 "32",LT,CALLDATASIZE,PUSH1 "04",POP]
[JUMPI,PUSH1 "37",EQ,PUSH4 "371303C0",DUP1,SHR,PUSH1 "E0",CALLDATALOAD,PUSH1 "00"]
[JUMPI,PUSH1 "3F",EQ,PUSH4 "6D4CE63C",DUP1]
[JUMPDEST "32"]
[REVERT,DUP1,PUSH1 "00"]
[JUMPDEST "37"]
[JUMP,PUSH1 "54",PUSH1 "3D"]
[JUMPDEST "3D"]
[STOP]
[JUMPDEST "3F"]
[RETURN,SWAP1,SUB,SWAP2,DUP1,MLOAD,PUSH1 "40",ADD,PUSH1 "20",MSTORE,DUP2,SWAP1,MLOAD,PUSH1 "40",SLOAD,PUSH1 "00"]
[JUMPDEST "54"]
[JUMP,PUSH1 "65",PUSH1 "01",SWAP1,PUSH1 "60",SLOAD,PUSH1 "00"]
[JUMPDEST "60"]
[JUMP,SSTORE,PUSH1 "00"]
[JUMPDEST "65"]
[JUMPI,PUSH1 "83",ISZERO,GT,DUP3,NOT,DUP3,PUSH1 "00"]
[REVERT,DUP2,PUSH1 "24",MSTORE,PUSH1 "04",PUSH1 "11",MSTORE,DUP2,SHL,PUSH1 "E0",PUSH4 "4E487B71"]
[JUMPDEST "83"]
[JUMP,SWAP1,ADD,POP]
[INVALID]
[INFO "33",INFO "00",INFO "000802",INFO "solc43",INFO "64",INFO "1220EBBF9E6530745C2246C5EC9DF0F41280941AB72D344700CCF6BFF96F2DF6469C",INFO "ipfs5822",INFO "64",INFO "A2"]
[
*-( SEQ
    *-( MSTORE
    |   +-  PUSH1 "80"
    |   +-  PUSH1 "40"
    +-  CALLVALUE
    *-( JUMPI
        +-  ISZERO
        |   +-  DUP1
        +-  PUSH2 "0010"
,
*-( SEQ
    +-  REVERT
        +-  PUSH1 "00"
        +-  DUP1
,
*-( SEQ
    *-( JUMPDEST "10"
    *-( POP
    |   +-  STACK 1
    +-  RETURN
        +-  PUSH1 "BE"
        *-( CODECOPY
        |   +-  DUP1
        |   +-  PUSH2 "001F"
        |   +-  PUSH1 "00"
        +-  PUSH1 "00"
,
*-( SEQ
    *-( INVALID
,
*-( SEQ
    *-( MSTORE
    |   +-  PUSH1 "80"
    |   +-  PUSH1 "40"
    +-  CALLVALUE
    *-( JUMPI
        +-  ISZERO
        |   +-  DUP1
        +-  PUSH1 "0F"
,
*-( SEQ
    +-  REVERT
        +-  PUSH1 "00"
        +-  DUP1
,
*-( SEQ
    *-( JUMPDEST "F"
    *-( POP
    |   +-  STACK 1
    *-( JUMPI
        +-  LT
        |   +-  PUSH1 "04"
        |   +-  CALLDATASIZE
        +-  PUSH1 "32"
,
*-( SEQ
    +-  SHR
    |   +-  CALLDATALOAD
    |   |   +-  PUSH1 "00"
    |   +-  PUSH1 "E0"
    *-( JUMPI
        +-  EQ
        |   +-  DUP1
        |   +-  PUSH4 "371303C0"
        +-  PUSH1 "37"
,
*-( SEQ
    *-( JUMPI
        +-  EQ
        |   +-  DUP1
        |   +-  PUSH4 "6D4CE63C"
        +-  PUSH1 "3F"
,
*-( SEQ
    *-( JUMPDEST "32"
    +-  REVERT
        +-  PUSH1 "00"
        +-  DUP1
,
*-( SEQ
    *-( JUMPDEST "37"
    +-  PUSH1 "3D"
    *-( JUMP
        +-  PUSH1 "54"
,
*-( SEQ
    *-( JUMPDEST "3D"
    *-( STOP
,
*-( SEQ
    *-( JUMPDEST "3F"
    +-  RETURN
        +-  ADD
        |   +-  SLOAD
        |   |   +-  PUSH1 "00"
        |   *-( MSTORE
        |   |   +-  MLOAD
        |   |   |   +-  PUSH1 "40"
        |   |   *-( SWAP1
        |   |   +-  DUP2
        |   +-  PUSH1 "20"
        +-  SUB
        |   +-  MLOAD
        |   |   +-  PUSH1 "40"
        |   +-  DUP1
        |   *-( SWAP2
        *-( SWAP1
,
*-( SEQ
    *-( JUMPDEST "54"
    +-  SLOAD
    |   +-  PUSH1 "00"
    +-  PUSH1 "60"
    *-( SWAP1
    +-  PUSH1 "01"
    *-( JUMP
        +-  PUSH1 "65"
,
*-( SEQ
    *-( JUMPDEST "60"
    *-( JUMP
        *-( SSTORE
        |   +-  STACK 1
        |   +-  PUSH1 "00"
        +-  STACK 2
,
*-( SEQ
    *-( JUMPDEST "65"
    +-  PUSH1 "00"
    *-( JUMPI
        +-  ISZERO
        |   +-  GT
        |       +-  NOT
        |       |   +-  DUP3
        |       +-  DUP3
        +-  PUSH1 "83"
,
*-( SEQ
    *-( MSTORE
    |   +-  SHL
    |   |   +-  PUSH4 "4E487B71"
    |   |   +-  PUSH1 "E0"
    |   +-  DUP2
    *-( MSTORE
    |   +-  PUSH1 "11"
    |   +-  PUSH1 "04"
    +-  REVERT
        +-  PUSH1 "24"
        +-  DUP2
,
*-( SEQ
    *-( JUMPDEST "83"
    *-( JUMP
        +-  ADD
        |   *-( POP
        |   |   +-  STACK 1
        |   +-  STACK 3
        |   +-  STACK 2
        *-( SWAP1
,
*-( SEQ
    *-( INVALID
,
*-( SEQ
]
