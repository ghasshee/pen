{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer 
import Prelude hiding (lookup, lex, EQ, LT, GT) 

import GCLL
import Predicate 
import Tree
import Type
import Term
import AST
import PsrCtx
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,552) ([0,0,8,0,0,0,0,0,256,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,128,0,0,2048,0,0,0,0,1024,0,0,0,0,2,0,0,32,0,0,64,0,0,1024,0,0,0,0,8192,0,0,0,0,0,0,0,16,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,1024,0,8,0,0,31744,33792,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,514,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1984,2112,0,0,0,0,0,0,128,0,0,0,0,0,32,16384,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,15872,16896,0,0,0,0,0,0,8210,0,0,0,0,0,512,0,0,0,0,7936,8448,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,66,0,0,0,0,0,512,16,0,0,0,63488,2048,1,0,0,0,0,0,72,0,0,0,0,0,4096,0,0,0,0,0,8192,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,2616,4672,32776,28800,8,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,511,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,81,16530,1024,132,0,0,2616,4672,32776,4224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41856,9216,129,2056,135,0,28672,32772,4132,0,32,0,0,142,1168,2,1024,0,0,0,0,0,0,0,0,14336,16394,2066,32896,3056,0,0,327,584,4097,32272,1,0,10464,18688,32,49666,33,0,7168,8197,1033,16448,8,0,16384,0,0,0,0,0,0,0,0,64,4094,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,2616,4672,32776,4224,0,0,18176,18432,258,0,2,0,0,0,0,0,0,0,0,0,0,32776,1023,0,0,0,0,128,0,1,0,0,0,0,0,0,0,0,654,1168,8194,1056,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32784,1023,0,0,0,0,0,0,0,0,0,0,0,0,34816,0,0,0,0,0,0,0,0,20928,37376,64,33796,67,0,14336,16394,2066,32896,2160,0,0,327,584,4097,3600,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,128,0,0,0,1024,65472,1,0,0,128,0,63488,63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14336,16394,2066,32896,2160,0,0,0,0,0,0,0,0,0,0,0,8188,0,0,0,0,0,65408,3,0,0,0,0,61440,127,0,0,0,0,0,4094,0,0,0,0,0,65472,1,0,49152,81,16530,1024,132,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28672,32788,4132,256,4321,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,327,584,4097,528,0,0,0,0,0,0,0,0,0,16,0,65408,3,0,32768,163,33060,2048,264,0,0,0,0,1024,4094,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","TopLevel","Contract","Top","Mthd","StoVars","Params","Param","IDs","Ty","Tys","ATy","Body","Decls","Decl","Predicate","Formulae","AppFormulae","PathFormulae","TFormulae","AFormulae","BOp","Tm","AppTm","PathTm","ATm","Num","Bool","let","true","false","num","event","contract","method","return","become","if","then","else","i8","u8","i256","u256","bool","case","new","call","sender","send","msg","amount","balance","dstrct","this","now","'()'","log","kec","'->'","'=>'","'('","')'","'{'","'}'","'['","']'","';'","':'","'='","':='","','","'.'","'~'","'>'","'<'","'>='","'<='","'=='","'!='","'+'","'-'","'*'","'/'","'%'","comment","id","E","A","X","F","G","U","always","never","by","and","or","%eof"]
        bit_start = st Prelude.* 101
        bit_end = (st Prelude.+ 1) Prelude.* 101
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..100]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (36) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (36) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (36) = happyShift action_3
action_2 (4) = happyGoto action_6
action_2 (5) = happyGoto action_2
action_2 _ = happyReduce_2

action_3 (89) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (101) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (66) = happyShift action_7
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_1

action_7 (37) = happyShift action_12
action_7 (71) = happyReduce_14
action_7 (89) = happyShift action_13
action_7 (6) = happyGoto action_8
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (11) = happyGoto action_11
action_7 _ = happyReduce_6

action_8 (67) = happyShift action_19
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (37) = happyShift action_12
action_9 (71) = happyReduce_14
action_9 (89) = happyShift action_13
action_9 (6) = happyGoto action_18
action_9 (7) = happyGoto action_9
action_9 (8) = happyGoto action_10
action_9 (11) = happyGoto action_11
action_9 _ = happyReduce_6

action_10 (37) = happyShift action_12
action_10 (71) = happyReduce_14
action_10 (89) = happyShift action_13
action_10 (6) = happyGoto action_17
action_10 (7) = happyGoto action_9
action_10 (8) = happyGoto action_10
action_10 (11) = happyGoto action_11
action_10 _ = happyReduce_6

action_11 (71) = happyShift action_16
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (89) = happyShift action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (89) = happyShift action_13
action_13 (11) = happyGoto action_14
action_13 _ = happyReduce_14

action_14 _ = happyReduce_13

action_15 (64) = happyShift action_31
action_15 (89) = happyShift action_32
action_15 (9) = happyGoto action_29
action_15 (10) = happyGoto action_30
action_15 _ = happyReduce_10

action_16 (43) = happyShift action_22
action_16 (44) = happyShift action_23
action_16 (45) = happyShift action_24
action_16 (46) = happyShift action_25
action_16 (47) = happyShift action_26
action_16 (59) = happyShift action_27
action_16 (64) = happyShift action_28
action_16 (12) = happyGoto action_20
action_16 (14) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_4

action_18 _ = happyReduce_5

action_19 _ = happyReduce_3

action_20 (62) = happyShift action_38
action_20 (70) = happyShift action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_15

action_22 _ = happyReduce_22

action_23 _ = happyReduce_21

action_24 _ = happyReduce_24

action_25 _ = happyReduce_23

action_26 _ = happyReduce_20

action_27 _ = happyReduce_25

action_28 (43) = happyShift action_22
action_28 (44) = happyShift action_23
action_28 (45) = happyShift action_24
action_28 (46) = happyShift action_25
action_28 (47) = happyShift action_26
action_28 (59) = happyShift action_27
action_28 (64) = happyShift action_28
action_28 (12) = happyGoto action_36
action_28 (13) = happyGoto action_37
action_28 (14) = happyGoto action_21
action_28 _ = happyReduce_19

action_29 (71) = happyShift action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (64) = happyShift action_31
action_30 (89) = happyShift action_32
action_30 (9) = happyGoto action_34
action_30 (10) = happyGoto action_30
action_30 _ = happyReduce_10

action_31 (89) = happyShift action_13
action_31 (11) = happyGoto action_33
action_31 _ = happyReduce_14

action_32 _ = happyReduce_12

action_33 (71) = happyShift action_45
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_9

action_35 (43) = happyShift action_22
action_35 (44) = happyShift action_23
action_35 (45) = happyShift action_24
action_35 (46) = happyShift action_25
action_35 (47) = happyShift action_26
action_35 (59) = happyShift action_27
action_35 (64) = happyShift action_28
action_35 (12) = happyGoto action_44
action_35 (14) = happyGoto action_21
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (62) = happyShift action_38
action_36 (65) = happyShift action_42
action_36 (74) = happyShift action_43
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (65) = happyShift action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (43) = happyShift action_22
action_38 (44) = happyShift action_23
action_38 (45) = happyShift action_24
action_38 (46) = happyShift action_25
action_38 (47) = happyShift action_26
action_38 (59) = happyShift action_27
action_38 (64) = happyShift action_28
action_38 (12) = happyGoto action_40
action_38 (14) = happyGoto action_21
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_8

action_40 (62) = happyShift action_38
action_40 _ = happyReduce_16

action_41 _ = happyReduce_17

action_42 _ = happyReduce_26

action_43 (43) = happyShift action_22
action_43 (44) = happyShift action_23
action_43 (45) = happyShift action_24
action_43 (46) = happyShift action_25
action_43 (47) = happyShift action_26
action_43 (59) = happyShift action_27
action_43 (64) = happyShift action_28
action_43 (12) = happyGoto action_48
action_43 (13) = happyGoto action_49
action_43 (14) = happyGoto action_21
action_43 _ = happyReduce_19

action_44 (62) = happyShift action_38
action_44 (73) = happyShift action_47
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (43) = happyShift action_22
action_45 (44) = happyShift action_23
action_45 (45) = happyShift action_24
action_45 (46) = happyShift action_25
action_45 (47) = happyShift action_26
action_45 (59) = happyShift action_27
action_45 (64) = happyShift action_28
action_45 (12) = happyGoto action_46
action_45 (14) = happyGoto action_21
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (62) = happyShift action_38
action_46 (65) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (66) = happyShift action_52
action_47 (15) = happyGoto action_50
action_47 (18) = happyGoto action_51
action_47 _ = happyReduce_33

action_48 (62) = happyShift action_38
action_48 (74) = happyShift action_43
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_18

action_50 _ = happyReduce_7

action_51 (31) = happyShift action_82
action_51 (32) = happyShift action_83
action_51 (33) = happyShift action_65
action_51 (34) = happyShift action_66
action_51 (38) = happyShift action_67
action_51 (40) = happyShift action_68
action_51 (51) = happyShift action_69
action_51 (54) = happyShift action_70
action_51 (57) = happyShift action_71
action_51 (64) = happyShift action_84
action_51 (76) = happyShift action_85
action_51 (84) = happyShift action_74
action_51 (89) = happyShift action_75
action_51 (16) = happyGoto action_79
action_51 (17) = happyGoto action_80
action_51 (25) = happyGoto action_81
action_51 (26) = happyGoto action_59
action_51 (27) = happyGoto action_60
action_51 (28) = happyGoto action_61
action_51 (29) = happyGoto action_62
action_51 (30) = happyGoto action_63
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (32) = happyShift action_64
action_52 (33) = happyShift action_65
action_52 (34) = happyShift action_66
action_52 (38) = happyShift action_67
action_52 (40) = happyShift action_68
action_52 (51) = happyShift action_69
action_52 (54) = happyShift action_70
action_52 (57) = happyShift action_71
action_52 (64) = happyShift action_72
action_52 (76) = happyShift action_73
action_52 (84) = happyShift action_74
action_52 (89) = happyShift action_75
action_52 (90) = happyShift action_76
action_52 (91) = happyShift action_77
action_52 (96) = happyShift action_78
action_52 (19) = happyGoto action_54
action_52 (20) = happyGoto action_55
action_52 (22) = happyGoto action_56
action_52 (23) = happyGoto action_57
action_52 (25) = happyGoto action_58
action_52 (26) = happyGoto action_59
action_52 (27) = happyGoto action_60
action_52 (28) = happyGoto action_61
action_52 (29) = happyGoto action_62
action_52 (30) = happyGoto action_63
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_11

action_54 (67) = happyShift action_123
action_54 (99) = happyShift action_124
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_38

action_56 _ = happyReduce_40

action_57 _ = happyReduce_50

action_58 (77) = happyShift action_118
action_58 (78) = happyShift action_119
action_58 (79) = happyShift action_120
action_58 (80) = happyShift action_121
action_58 (81) = happyShift action_122
action_58 (82) = happyShift action_96
action_58 (83) = happyShift action_97
action_58 (84) = happyShift action_98
action_58 (85) = happyShift action_99
action_58 (86) = happyShift action_100
action_58 (87) = happyShift action_101
action_58 (24) = happyGoto action_90
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (32) = happyShift action_83
action_59 (33) = happyShift action_65
action_59 (34) = happyShift action_66
action_59 (38) = happyShift action_67
action_59 (51) = happyShift action_69
action_59 (54) = happyShift action_70
action_59 (57) = happyShift action_71
action_59 (64) = happyShift action_84
action_59 (89) = happyShift action_75
action_59 (27) = happyGoto action_117
action_59 (28) = happyGoto action_61
action_59 (29) = happyGoto action_62
action_59 (30) = happyGoto action_63
action_59 _ = happyReduce_66

action_60 _ = happyReduce_67

action_61 _ = happyReduce_71

action_62 _ = happyReduce_74

action_63 _ = happyReduce_75

action_64 (65) = happyReduce_81
action_64 (67) = happyReduce_52
action_64 (95) = happyReduce_52
action_64 (99) = happyReduce_52
action_64 _ = happyReduce_81

action_65 _ = happyReduce_82

action_66 _ = happyReduce_80

action_67 (32) = happyShift action_83
action_67 (33) = happyShift action_65
action_67 (34) = happyShift action_66
action_67 (38) = happyShift action_67
action_67 (40) = happyShift action_68
action_67 (51) = happyShift action_69
action_67 (54) = happyShift action_70
action_67 (57) = happyShift action_71
action_67 (64) = happyShift action_84
action_67 (76) = happyShift action_85
action_67 (84) = happyShift action_74
action_67 (89) = happyShift action_75
action_67 (25) = happyGoto action_116
action_67 (26) = happyGoto action_59
action_67 (27) = happyGoto action_60
action_67 (28) = happyGoto action_61
action_67 (29) = happyGoto action_62
action_67 (30) = happyGoto action_63
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (32) = happyShift action_83
action_68 (33) = happyShift action_65
action_68 (34) = happyShift action_66
action_68 (38) = happyShift action_67
action_68 (40) = happyShift action_68
action_68 (51) = happyShift action_69
action_68 (54) = happyShift action_70
action_68 (57) = happyShift action_71
action_68 (64) = happyShift action_84
action_68 (76) = happyShift action_85
action_68 (84) = happyShift action_74
action_68 (89) = happyShift action_75
action_68 (25) = happyGoto action_115
action_68 (26) = happyGoto action_59
action_68 (27) = happyGoto action_60
action_68 (28) = happyGoto action_61
action_68 (29) = happyGoto action_62
action_68 (30) = happyGoto action_63
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_78

action_70 _ = happyReduce_76

action_71 _ = happyReduce_77

action_72 (32) = happyShift action_64
action_72 (33) = happyShift action_65
action_72 (34) = happyShift action_66
action_72 (38) = happyShift action_67
action_72 (40) = happyShift action_68
action_72 (51) = happyShift action_69
action_72 (54) = happyShift action_70
action_72 (57) = happyShift action_71
action_72 (64) = happyShift action_72
action_72 (76) = happyShift action_73
action_72 (84) = happyShift action_74
action_72 (89) = happyShift action_75
action_72 (90) = happyShift action_76
action_72 (91) = happyShift action_77
action_72 (96) = happyShift action_78
action_72 (19) = happyGoto action_113
action_72 (20) = happyGoto action_55
action_72 (22) = happyGoto action_56
action_72 (23) = happyGoto action_57
action_72 (25) = happyGoto action_114
action_72 (26) = happyGoto action_59
action_72 (27) = happyGoto action_60
action_72 (28) = happyGoto action_61
action_72 (29) = happyGoto action_62
action_72 (30) = happyGoto action_63
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (32) = happyShift action_64
action_73 (33) = happyShift action_65
action_73 (34) = happyShift action_66
action_73 (38) = happyShift action_67
action_73 (51) = happyShift action_69
action_73 (54) = happyShift action_70
action_73 (57) = happyShift action_71
action_73 (64) = happyShift action_72
action_73 (89) = happyShift action_75
action_73 (23) = happyGoto action_112
action_73 (27) = happyGoto action_86
action_73 (28) = happyGoto action_61
action_73 (29) = happyGoto action_62
action_73 (30) = happyGoto action_63
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (32) = happyShift action_83
action_74 (33) = happyShift action_65
action_74 (34) = happyShift action_66
action_74 (38) = happyShift action_67
action_74 (51) = happyShift action_69
action_74 (54) = happyShift action_70
action_74 (57) = happyShift action_71
action_74 (64) = happyShift action_84
action_74 (89) = happyShift action_75
action_74 (27) = happyGoto action_111
action_74 (28) = happyGoto action_61
action_74 (29) = happyGoto action_62
action_74 (30) = happyGoto action_63
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_79

action_76 (32) = happyShift action_64
action_76 (33) = happyShift action_65
action_76 (34) = happyShift action_66
action_76 (38) = happyShift action_67
action_76 (40) = happyShift action_68
action_76 (51) = happyShift action_69
action_76 (54) = happyShift action_70
action_76 (57) = happyShift action_71
action_76 (64) = happyShift action_72
action_76 (76) = happyShift action_73
action_76 (84) = happyShift action_74
action_76 (89) = happyShift action_75
action_76 (90) = happyShift action_76
action_76 (91) = happyShift action_77
action_76 (92) = happyShift action_107
action_76 (93) = happyShift action_108
action_76 (94) = happyShift action_109
action_76 (96) = happyShift action_78
action_76 (19) = happyGoto action_105
action_76 (20) = happyGoto action_55
action_76 (21) = happyGoto action_110
action_76 (22) = happyGoto action_56
action_76 (23) = happyGoto action_57
action_76 (25) = happyGoto action_58
action_76 (26) = happyGoto action_59
action_76 (27) = happyGoto action_60
action_76 (28) = happyGoto action_61
action_76 (29) = happyGoto action_62
action_76 (30) = happyGoto action_63
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (32) = happyShift action_64
action_77 (33) = happyShift action_65
action_77 (34) = happyShift action_66
action_77 (38) = happyShift action_67
action_77 (40) = happyShift action_68
action_77 (51) = happyShift action_69
action_77 (54) = happyShift action_70
action_77 (57) = happyShift action_71
action_77 (64) = happyShift action_72
action_77 (76) = happyShift action_73
action_77 (84) = happyShift action_74
action_77 (89) = happyShift action_75
action_77 (90) = happyShift action_76
action_77 (91) = happyShift action_77
action_77 (92) = happyShift action_107
action_77 (93) = happyShift action_108
action_77 (94) = happyShift action_109
action_77 (96) = happyShift action_78
action_77 (19) = happyGoto action_105
action_77 (20) = happyGoto action_55
action_77 (21) = happyGoto action_106
action_77 (22) = happyGoto action_56
action_77 (23) = happyGoto action_57
action_77 (25) = happyGoto action_58
action_77 (26) = happyGoto action_59
action_77 (27) = happyGoto action_60
action_77 (28) = happyGoto action_61
action_77 (29) = happyGoto action_62
action_77 (30) = happyGoto action_63
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (32) = happyShift action_64
action_78 (33) = happyShift action_65
action_78 (34) = happyShift action_66
action_78 (38) = happyShift action_67
action_78 (40) = happyShift action_68
action_78 (51) = happyShift action_69
action_78 (54) = happyShift action_70
action_78 (57) = happyShift action_71
action_78 (64) = happyShift action_72
action_78 (76) = happyShift action_73
action_78 (84) = happyShift action_74
action_78 (89) = happyShift action_75
action_78 (90) = happyShift action_76
action_78 (91) = happyShift action_77
action_78 (96) = happyShift action_78
action_78 (19) = happyGoto action_104
action_78 (20) = happyGoto action_55
action_78 (22) = happyGoto action_56
action_78 (23) = happyGoto action_57
action_78 (25) = happyGoto action_58
action_78 (26) = happyGoto action_59
action_78 (27) = happyGoto action_60
action_78 (28) = happyGoto action_61
action_78 (29) = happyGoto action_62
action_78 (30) = happyGoto action_63
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (32) = happyShift action_83
action_79 (33) = happyShift action_65
action_79 (34) = happyShift action_66
action_79 (38) = happyShift action_67
action_79 (40) = happyShift action_68
action_79 (51) = happyShift action_69
action_79 (54) = happyShift action_70
action_79 (57) = happyShift action_71
action_79 (64) = happyShift action_84
action_79 (76) = happyShift action_85
action_79 (84) = happyShift action_74
action_79 (89) = happyShift action_75
action_79 (25) = happyGoto action_103
action_79 (26) = happyGoto action_59
action_79 (27) = happyGoto action_60
action_79 (28) = happyGoto action_61
action_79 (29) = happyGoto action_62
action_79 (30) = happyGoto action_63
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (31) = happyShift action_82
action_80 (16) = happyGoto action_102
action_80 (17) = happyGoto action_80
action_80 _ = happyReduce_30

action_81 (66) = happyShift action_52
action_81 (77) = happyShift action_91
action_81 (78) = happyShift action_92
action_81 (79) = happyShift action_93
action_81 (80) = happyShift action_94
action_81 (81) = happyShift action_95
action_81 (82) = happyShift action_96
action_81 (83) = happyShift action_97
action_81 (84) = happyShift action_98
action_81 (85) = happyShift action_99
action_81 (86) = happyShift action_100
action_81 (87) = happyShift action_101
action_81 (18) = happyGoto action_89
action_81 (24) = happyGoto action_90
action_81 _ = happyReduce_33

action_82 (89) = happyShift action_88
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_81

action_84 (32) = happyShift action_83
action_84 (33) = happyShift action_65
action_84 (34) = happyShift action_66
action_84 (38) = happyShift action_67
action_84 (40) = happyShift action_68
action_84 (51) = happyShift action_69
action_84 (54) = happyShift action_70
action_84 (57) = happyShift action_71
action_84 (64) = happyShift action_84
action_84 (76) = happyShift action_85
action_84 (84) = happyShift action_74
action_84 (89) = happyShift action_75
action_84 (25) = happyGoto action_87
action_84 (26) = happyGoto action_59
action_84 (27) = happyGoto action_60
action_84 (28) = happyGoto action_61
action_84 (29) = happyGoto action_62
action_84 (30) = happyGoto action_63
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (32) = happyShift action_83
action_85 (33) = happyShift action_65
action_85 (34) = happyShift action_66
action_85 (38) = happyShift action_67
action_85 (51) = happyShift action_69
action_85 (54) = happyShift action_70
action_85 (57) = happyShift action_71
action_85 (64) = happyShift action_84
action_85 (89) = happyShift action_75
action_85 (27) = happyGoto action_86
action_85 (28) = happyGoto action_61
action_85 (29) = happyGoto action_62
action_85 (30) = happyGoto action_63
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_68

action_87 (65) = happyShift action_132
action_87 (77) = happyShift action_91
action_87 (78) = happyShift action_92
action_87 (79) = happyShift action_93
action_87 (80) = happyShift action_94
action_87 (81) = happyShift action_95
action_87 (82) = happyShift action_96
action_87 (83) = happyShift action_97
action_87 (84) = happyShift action_98
action_87 (85) = happyShift action_99
action_87 (86) = happyShift action_100
action_87 (87) = happyShift action_101
action_87 (24) = happyGoto action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (64) = happyShift action_31
action_88 (89) = happyShift action_32
action_88 (9) = happyGoto action_140
action_88 (10) = happyGoto action_30
action_88 _ = happyReduce_10

action_89 _ = happyReduce_27

action_90 (32) = happyShift action_83
action_90 (33) = happyShift action_65
action_90 (34) = happyShift action_66
action_90 (38) = happyShift action_67
action_90 (40) = happyShift action_68
action_90 (51) = happyShift action_69
action_90 (54) = happyShift action_70
action_90 (57) = happyShift action_71
action_90 (64) = happyShift action_84
action_90 (76) = happyShift action_85
action_90 (84) = happyShift action_74
action_90 (89) = happyShift action_75
action_90 (25) = happyGoto action_139
action_90 (26) = happyGoto action_59
action_90 (27) = happyGoto action_60
action_90 (28) = happyGoto action_61
action_90 (29) = happyGoto action_62
action_90 (30) = happyGoto action_63
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_56

action_92 _ = happyReduce_55

action_93 _ = happyReduce_58

action_94 _ = happyReduce_57

action_95 _ = happyReduce_53

action_96 _ = happyReduce_54

action_97 _ = happyReduce_59

action_98 _ = happyReduce_60

action_99 _ = happyReduce_61

action_100 _ = happyReduce_62

action_101 _ = happyReduce_63

action_102 _ = happyReduce_29

action_103 (66) = happyShift action_52
action_103 (77) = happyShift action_91
action_103 (78) = happyShift action_92
action_103 (79) = happyShift action_93
action_103 (80) = happyShift action_94
action_103 (81) = happyShift action_95
action_103 (82) = happyShift action_96
action_103 (83) = happyShift action_97
action_103 (84) = happyShift action_98
action_103 (85) = happyShift action_99
action_103 (86) = happyShift action_100
action_103 (87) = happyShift action_101
action_103 (18) = happyGoto action_138
action_103 (24) = happyGoto action_90
action_103 _ = happyReduce_33

action_104 (99) = happyShift action_124
action_104 _ = happyReduce_37

action_105 (95) = happyShift action_137
action_105 (99) = happyShift action_124
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_36

action_107 (32) = happyShift action_64
action_107 (33) = happyShift action_65
action_107 (34) = happyShift action_66
action_107 (38) = happyShift action_67
action_107 (40) = happyShift action_68
action_107 (51) = happyShift action_69
action_107 (54) = happyShift action_70
action_107 (57) = happyShift action_71
action_107 (64) = happyShift action_72
action_107 (76) = happyShift action_73
action_107 (84) = happyShift action_74
action_107 (89) = happyShift action_75
action_107 (90) = happyShift action_76
action_107 (91) = happyShift action_77
action_107 (96) = happyShift action_78
action_107 (19) = happyGoto action_136
action_107 (20) = happyGoto action_55
action_107 (22) = happyGoto action_56
action_107 (23) = happyGoto action_57
action_107 (25) = happyGoto action_58
action_107 (26) = happyGoto action_59
action_107 (27) = happyGoto action_60
action_107 (28) = happyGoto action_61
action_107 (29) = happyGoto action_62
action_107 (30) = happyGoto action_63
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (32) = happyShift action_64
action_108 (33) = happyShift action_65
action_108 (34) = happyShift action_66
action_108 (38) = happyShift action_67
action_108 (40) = happyShift action_68
action_108 (51) = happyShift action_69
action_108 (54) = happyShift action_70
action_108 (57) = happyShift action_71
action_108 (64) = happyShift action_72
action_108 (76) = happyShift action_73
action_108 (84) = happyShift action_74
action_108 (89) = happyShift action_75
action_108 (90) = happyShift action_76
action_108 (91) = happyShift action_77
action_108 (96) = happyShift action_78
action_108 (19) = happyGoto action_135
action_108 (20) = happyGoto action_55
action_108 (22) = happyGoto action_56
action_108 (23) = happyGoto action_57
action_108 (25) = happyGoto action_58
action_108 (26) = happyGoto action_59
action_108 (27) = happyGoto action_60
action_108 (28) = happyGoto action_61
action_108 (29) = happyGoto action_62
action_108 (30) = happyGoto action_63
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (32) = happyShift action_64
action_109 (33) = happyShift action_65
action_109 (34) = happyShift action_66
action_109 (38) = happyShift action_67
action_109 (40) = happyShift action_68
action_109 (51) = happyShift action_69
action_109 (54) = happyShift action_70
action_109 (57) = happyShift action_71
action_109 (64) = happyShift action_72
action_109 (76) = happyShift action_73
action_109 (84) = happyShift action_74
action_109 (89) = happyShift action_75
action_109 (90) = happyShift action_76
action_109 (91) = happyShift action_77
action_109 (96) = happyShift action_78
action_109 (19) = happyGoto action_134
action_109 (20) = happyGoto action_55
action_109 (22) = happyGoto action_56
action_109 (23) = happyGoto action_57
action_109 (25) = happyGoto action_58
action_109 (26) = happyGoto action_59
action_109 (27) = happyGoto action_60
action_109 (28) = happyGoto action_61
action_109 (29) = happyGoto action_62
action_109 (30) = happyGoto action_63
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_35

action_111 _ = happyReduce_69

action_112 _ = happyReduce_34

action_113 (65) = happyShift action_133
action_113 (99) = happyShift action_124
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (65) = happyShift action_132
action_114 (77) = happyShift action_118
action_114 (78) = happyShift action_119
action_114 (79) = happyShift action_120
action_114 (80) = happyShift action_121
action_114 (81) = happyShift action_122
action_114 (82) = happyShift action_96
action_114 (83) = happyShift action_97
action_114 (84) = happyShift action_98
action_114 (85) = happyShift action_99
action_114 (86) = happyShift action_100
action_114 (87) = happyShift action_101
action_114 (24) = happyGoto action_90
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (41) = happyShift action_131
action_115 (77) = happyShift action_91
action_115 (78) = happyShift action_92
action_115 (79) = happyShift action_93
action_115 (80) = happyShift action_94
action_115 (81) = happyShift action_95
action_115 (82) = happyShift action_96
action_115 (83) = happyShift action_97
action_115 (84) = happyShift action_98
action_115 (85) = happyShift action_99
action_115 (86) = happyShift action_100
action_115 (87) = happyShift action_101
action_115 (24) = happyGoto action_90
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (77) = happyShift action_91
action_116 (78) = happyShift action_92
action_116 (79) = happyShift action_93
action_116 (80) = happyShift action_94
action_116 (81) = happyShift action_95
action_116 (82) = happyShift action_96
action_116 (83) = happyShift action_97
action_116 (84) = happyShift action_98
action_116 (85) = happyShift action_99
action_116 (86) = happyShift action_100
action_116 (87) = happyShift action_101
action_116 (24) = happyGoto action_90
action_116 _ = happyReduce_73

action_117 _ = happyReduce_70

action_118 (32) = happyShift action_83
action_118 (33) = happyShift action_65
action_118 (34) = happyShift action_66
action_118 (38) = happyShift action_67
action_118 (40) = happyShift action_68
action_118 (51) = happyShift action_69
action_118 (54) = happyShift action_70
action_118 (57) = happyShift action_71
action_118 (64) = happyShift action_84
action_118 (76) = happyShift action_85
action_118 (84) = happyShift action_74
action_118 (89) = happyShift action_75
action_118 (25) = happyGoto action_130
action_118 (26) = happyGoto action_59
action_118 (27) = happyGoto action_60
action_118 (28) = happyGoto action_61
action_118 (29) = happyGoto action_62
action_118 (30) = happyGoto action_63
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (32) = happyShift action_83
action_119 (33) = happyShift action_65
action_119 (34) = happyShift action_66
action_119 (38) = happyShift action_67
action_119 (40) = happyShift action_68
action_119 (51) = happyShift action_69
action_119 (54) = happyShift action_70
action_119 (57) = happyShift action_71
action_119 (64) = happyShift action_84
action_119 (76) = happyShift action_85
action_119 (84) = happyShift action_74
action_119 (89) = happyShift action_75
action_119 (25) = happyGoto action_129
action_119 (26) = happyGoto action_59
action_119 (27) = happyGoto action_60
action_119 (28) = happyGoto action_61
action_119 (29) = happyGoto action_62
action_119 (30) = happyGoto action_63
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (32) = happyShift action_83
action_120 (33) = happyShift action_65
action_120 (34) = happyShift action_66
action_120 (38) = happyShift action_67
action_120 (40) = happyShift action_68
action_120 (51) = happyShift action_69
action_120 (54) = happyShift action_70
action_120 (57) = happyShift action_71
action_120 (64) = happyShift action_84
action_120 (76) = happyShift action_85
action_120 (84) = happyShift action_74
action_120 (89) = happyShift action_75
action_120 (25) = happyGoto action_128
action_120 (26) = happyGoto action_59
action_120 (27) = happyGoto action_60
action_120 (28) = happyGoto action_61
action_120 (29) = happyGoto action_62
action_120 (30) = happyGoto action_63
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (32) = happyShift action_83
action_121 (33) = happyShift action_65
action_121 (34) = happyShift action_66
action_121 (38) = happyShift action_67
action_121 (40) = happyShift action_68
action_121 (51) = happyShift action_69
action_121 (54) = happyShift action_70
action_121 (57) = happyShift action_71
action_121 (64) = happyShift action_84
action_121 (76) = happyShift action_85
action_121 (84) = happyShift action_74
action_121 (89) = happyShift action_75
action_121 (25) = happyGoto action_127
action_121 (26) = happyGoto action_59
action_121 (27) = happyGoto action_60
action_121 (28) = happyGoto action_61
action_121 (29) = happyGoto action_62
action_121 (30) = happyGoto action_63
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (32) = happyShift action_83
action_122 (33) = happyShift action_65
action_122 (34) = happyShift action_66
action_122 (38) = happyShift action_67
action_122 (40) = happyShift action_68
action_122 (51) = happyShift action_69
action_122 (54) = happyShift action_70
action_122 (57) = happyShift action_71
action_122 (64) = happyShift action_84
action_122 (76) = happyShift action_85
action_122 (84) = happyShift action_74
action_122 (89) = happyShift action_75
action_122 (25) = happyGoto action_126
action_122 (26) = happyGoto action_59
action_122 (27) = happyGoto action_60
action_122 (28) = happyGoto action_61
action_122 (29) = happyGoto action_62
action_122 (30) = happyGoto action_63
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_32

action_124 (32) = happyShift action_64
action_124 (33) = happyShift action_65
action_124 (34) = happyShift action_66
action_124 (38) = happyShift action_67
action_124 (40) = happyShift action_68
action_124 (51) = happyShift action_69
action_124 (54) = happyShift action_70
action_124 (57) = happyShift action_71
action_124 (64) = happyShift action_72
action_124 (76) = happyShift action_73
action_124 (84) = happyShift action_74
action_124 (89) = happyShift action_75
action_124 (90) = happyShift action_76
action_124 (91) = happyShift action_77
action_124 (96) = happyShift action_78
action_124 (19) = happyGoto action_125
action_124 (20) = happyGoto action_55
action_124 (22) = happyGoto action_56
action_124 (23) = happyGoto action_57
action_124 (25) = happyGoto action_58
action_124 (26) = happyGoto action_59
action_124 (27) = happyGoto action_60
action_124 (28) = happyGoto action_61
action_124 (29) = happyGoto action_62
action_124 (30) = happyGoto action_63
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (99) = happyShift action_124
action_125 _ = happyReduce_39

action_126 (77) = happyShift action_91
action_126 (78) = happyShift action_92
action_126 (79) = happyShift action_93
action_126 (80) = happyShift action_94
action_126 (81) = happyShift action_95
action_126 (82) = happyShift action_96
action_126 (83) = happyShift action_97
action_126 (84) = happyShift action_98
action_126 (85) = happyShift action_99
action_126 (86) = happyShift action_100
action_126 (87) = happyShift action_101
action_126 (24) = happyGoto action_90
action_126 _ = happyReduce_45

action_127 (77) = happyShift action_91
action_127 (78) = happyShift action_92
action_127 (79) = happyShift action_93
action_127 (80) = happyShift action_94
action_127 (81) = happyShift action_95
action_127 (82) = happyShift action_96
action_127 (83) = happyShift action_97
action_127 (84) = happyShift action_98
action_127 (85) = happyShift action_99
action_127 (86) = happyShift action_100
action_127 (87) = happyShift action_101
action_127 (24) = happyGoto action_90
action_127 _ = happyReduce_48

action_128 (77) = happyShift action_91
action_128 (78) = happyShift action_92
action_128 (79) = happyShift action_93
action_128 (80) = happyShift action_94
action_128 (81) = happyShift action_95
action_128 (82) = happyShift action_96
action_128 (83) = happyShift action_97
action_128 (84) = happyShift action_98
action_128 (85) = happyShift action_99
action_128 (86) = happyShift action_100
action_128 (87) = happyShift action_101
action_128 (24) = happyGoto action_90
action_128 _ = happyReduce_49

action_129 (77) = happyShift action_91
action_129 (78) = happyShift action_92
action_129 (79) = happyShift action_93
action_129 (80) = happyShift action_94
action_129 (81) = happyShift action_95
action_129 (82) = happyShift action_96
action_129 (83) = happyShift action_97
action_129 (84) = happyShift action_98
action_129 (85) = happyShift action_99
action_129 (86) = happyShift action_100
action_129 (87) = happyShift action_101
action_129 (24) = happyGoto action_90
action_129 _ = happyReduce_46

action_130 (77) = happyShift action_91
action_130 (78) = happyShift action_92
action_130 (79) = happyShift action_93
action_130 (80) = happyShift action_94
action_130 (81) = happyShift action_95
action_130 (82) = happyShift action_96
action_130 (83) = happyShift action_97
action_130 (84) = happyShift action_98
action_130 (85) = happyShift action_99
action_130 (86) = happyShift action_100
action_130 (87) = happyShift action_101
action_130 (24) = happyGoto action_90
action_130 _ = happyReduce_47

action_131 (32) = happyShift action_83
action_131 (33) = happyShift action_65
action_131 (34) = happyShift action_66
action_131 (38) = happyShift action_67
action_131 (40) = happyShift action_68
action_131 (51) = happyShift action_69
action_131 (54) = happyShift action_70
action_131 (57) = happyShift action_71
action_131 (64) = happyShift action_84
action_131 (76) = happyShift action_85
action_131 (84) = happyShift action_74
action_131 (89) = happyShift action_75
action_131 (25) = happyGoto action_143
action_131 (26) = happyGoto action_59
action_131 (27) = happyGoto action_60
action_131 (28) = happyGoto action_61
action_131 (29) = happyGoto action_62
action_131 (30) = happyGoto action_63
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_72

action_133 _ = happyReduce_51

action_134 (99) = happyShift action_124
action_134 _ = happyReduce_43

action_135 (99) = happyShift action_124
action_135 _ = happyReduce_42

action_136 (99) = happyShift action_124
action_136 _ = happyReduce_41

action_137 (32) = happyShift action_64
action_137 (33) = happyShift action_65
action_137 (34) = happyShift action_66
action_137 (38) = happyShift action_67
action_137 (40) = happyShift action_68
action_137 (51) = happyShift action_69
action_137 (54) = happyShift action_70
action_137 (57) = happyShift action_71
action_137 (64) = happyShift action_72
action_137 (76) = happyShift action_73
action_137 (84) = happyShift action_74
action_137 (89) = happyShift action_75
action_137 (90) = happyShift action_76
action_137 (91) = happyShift action_77
action_137 (96) = happyShift action_78
action_137 (19) = happyGoto action_142
action_137 (20) = happyGoto action_55
action_137 (22) = happyGoto action_56
action_137 (23) = happyGoto action_57
action_137 (25) = happyGoto action_58
action_137 (26) = happyGoto action_59
action_137 (27) = happyGoto action_60
action_137 (28) = happyGoto action_61
action_137 (29) = happyGoto action_62
action_137 (30) = happyGoto action_63
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_28

action_139 (77) = happyShift action_91
action_139 (78) = happyShift action_92
action_139 (79) = happyShift action_93
action_139 (80) = happyShift action_94
action_139 (81) = happyShift action_95
action_139 (82) = happyShift action_96
action_139 (83) = happyShift action_97
action_139 (84) = happyShift action_98
action_139 (85) = happyShift action_99
action_139 (86) = happyShift action_100
action_139 (87) = happyShift action_101
action_139 (24) = happyGoto action_90
action_139 _ = happyReduce_65

action_140 (72) = happyShift action_141
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (32) = happyShift action_83
action_141 (33) = happyShift action_65
action_141 (34) = happyShift action_66
action_141 (38) = happyShift action_67
action_141 (40) = happyShift action_68
action_141 (51) = happyShift action_69
action_141 (54) = happyShift action_70
action_141 (57) = happyShift action_71
action_141 (64) = happyShift action_84
action_141 (76) = happyShift action_85
action_141 (84) = happyShift action_74
action_141 (89) = happyShift action_75
action_141 (25) = happyGoto action_145
action_141 (26) = happyGoto action_59
action_141 (27) = happyGoto action_60
action_141 (28) = happyGoto action_61
action_141 (29) = happyGoto action_62
action_141 (30) = happyGoto action_63
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (99) = happyShift action_124
action_142 _ = happyReduce_44

action_143 (42) = happyShift action_144
action_143 (77) = happyShift action_91
action_143 (78) = happyShift action_92
action_143 (79) = happyShift action_93
action_143 (80) = happyShift action_94
action_143 (81) = happyShift action_95
action_143 (82) = happyShift action_96
action_143 (83) = happyShift action_97
action_143 (84) = happyShift action_98
action_143 (85) = happyShift action_99
action_143 (86) = happyShift action_100
action_143 (87) = happyShift action_101
action_143 (24) = happyGoto action_90
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (32) = happyShift action_83
action_144 (33) = happyShift action_65
action_144 (34) = happyShift action_66
action_144 (38) = happyShift action_67
action_144 (40) = happyShift action_68
action_144 (51) = happyShift action_69
action_144 (54) = happyShift action_70
action_144 (57) = happyShift action_71
action_144 (64) = happyShift action_84
action_144 (76) = happyShift action_85
action_144 (84) = happyShift action_74
action_144 (89) = happyShift action_75
action_144 (25) = happyGoto action_147
action_144 (26) = happyGoto action_59
action_144 (27) = happyGoto action_60
action_144 (28) = happyGoto action_61
action_144 (29) = happyGoto action_62
action_144 (30) = happyGoto action_63
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (70) = happyShift action_146
action_145 (77) = happyShift action_91
action_145 (78) = happyShift action_92
action_145 (79) = happyShift action_93
action_145 (80) = happyShift action_94
action_145 (81) = happyShift action_95
action_145 (82) = happyShift action_96
action_145 (83) = happyShift action_97
action_145 (84) = happyShift action_98
action_145 (85) = happyShift action_99
action_145 (86) = happyShift action_100
action_145 (87) = happyShift action_101
action_145 (24) = happyGoto action_90
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (66) = happyShift action_52
action_146 (18) = happyGoto action_148
action_146 _ = happyReduce_33

action_147 (77) = happyShift action_91
action_147 (78) = happyShift action_92
action_147 (79) = happyShift action_93
action_147 (80) = happyShift action_94
action_147 (81) = happyShift action_95
action_147 (82) = happyShift action_96
action_147 (83) = happyShift action_97
action_147 (84) = happyShift action_98
action_147 (85) = happyShift action_99
action_147 (86) = happyShift action_100
action_147 (87) = happyShift action_101
action_147 (24) = happyGoto action_90
action_147 _ = happyReduce_64

action_148 _ = happyReduce_31

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happyReduce 5 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CN happy_var_2 (fst (happy_var_4 emptyCtx))
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (\ctx  ->  let (svs, ctx')     = happy_var_1 ctx    in
                                                    let (top, ctx'')    = happy_var_2 ctx'   in 
                                                    (svs ++ top        , ctx'')
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (\ctx ->   (happy_var_1 ctx : fst(happy_var_2 ctx) , ctx)
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  6 happyReduction_6
happyReduction_6  =  HappyAbsSyn6
		 (\ctx ->   ([],               ctx  )
	)

happyReduce_7 = happyReduce 7 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\ctx -> 
                                            let (params,ctx') = happy_var_3 ctx  in 
                                            MT happy_var_2 happy_var_5 params (happy_var_7 ctx')
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (\ctx ->   mapStoTy happy_var_3 happy_var_1 ctx
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\ctx -> 
                                            let (param,ctx')   = happy_var_1 ctx in 
                                            let (params,ctx'') = happy_var_2 ctx' in 
                                            (param ++ params , ctx'')
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 (\ctx -> ([],ctx)
	)

happyReduce_11 = happyReduce 5 10 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (\ctx -> mapParamTy happy_var_4       happy_var_2  ctx
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn10
		 (\ctx -> mapParamTy Untyped [happy_var_1] ctx
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  11 happyReduction_14
happyReduction_14  =  HappyAbsSyn11
		 ([]
	)

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (TyABS happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (TyPROD happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  13 happyReduction_19
happyReduction_19  =  HappyAbsSyn13
		 ([]
	)

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn14
		 (TyBOOL
	)

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn14
		 (TyU8
	)

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn14
		 (TyI8
	)

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn14
		 (TyU256
	)

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn14
		 (TyI256
	)

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn14
		 (TyUNIT
	)

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (\ctx -> 
                                            let (tm, ctx')   = happy_var_2 ctx in 
                                            BODY (happy_var_1 ctx') [] tm (happy_var_3 ctx')
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 15 happyReduction_28
happyReduction_28 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (\ctx -> 
                                            let (decls,ctx') = happy_var_2 ctx  in 
                                            let (tm,  ctx'') = happy_var_3 ctx' in
                                            BODY (happy_var_1 ctx'') decls tm (happy_var_4 ctx'')
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_2  16 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (\ctx -> 
                                            let (decl, ctx')    = happy_var_1 ctx in 
                                            let (decls, ctx'')  = happy_var_2 ctx' in 
                                            (decl:decls, ctx'')
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  16 happyReduction_30
happyReduction_30  =  HappyAbsSyn16
		 (\ctx -> ([], ctx)
	)

happyReduce_31 = happyReduce 7 17 happyReduction_31
happyReduction_31 ((HappyAbsSyn18  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (\ctx -> 
                                            case lookup' happy_var_2 ctx of 
                                            Just (TmSTO n)  -> 
                                                let id'  = '\'':happy_var_2 in 
                                                let ctx' = addCtx STO happy_var_2 ctx in 
                                                let ctx'''' = addCtx STO id' ctx in 
                                                let ([], ctx'') = happy_var_3 ctx' in 
                                                let (tm, ctx''')    = happy_var_5 ctx'''' in 
                                                (SLET happy_var_2 tm (happy_var_7 ctx'''), ctx')
                                            _               -> 
                                                let ctx' = addCtx VAR happy_var_2 ctx in 
                                                let (params, ctx'') = happy_var_3 ctx' in 
                                                case params of 
                                                []          -> 
                                                    let (tm, ctx''')   = happy_var_5 ctx''  in 
                                                    (LET  happy_var_2 tm (happy_var_7 ctx'''), ctx') 
                                                _           -> 
                                                    let (tm, ctx''')   = happy_var_5 ctx''  in 
                                                    (FLET happy_var_2 params tm (happy_var_7 ctx'''), ctx')
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (\ctx -> Just (happy_var_2 ctx)
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  18 happyReduction_33
happyReduction_33  =  HappyAbsSyn18
		 (\ctx -> Nothing
	)

happyReduce_34 = happySpecReduce_2  19 happyReduction_34
happyReduction_34 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> FNot (happy_var_2 ctx)
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  19 happyReduction_35
happyReduction_35 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> E    (happy_var_2 ctx)
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  19 happyReduction_36
happyReduction_36 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> A    (happy_var_2 ctx)
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> A (G (happy_var_2 ctx))
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  19 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  20 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn20
		 (\ctx -> FAnd (happy_var_1 ctx) (happy_var_3 ctx)
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn20
		 (\ctx -> happy_var_1 ctx
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  21 happyReduction_41
happyReduction_41 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (\ctx -> X     (happy_var_2 ctx)
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  21 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (\ctx -> F     (happy_var_2 ctx)
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  21 happyReduction_43
happyReduction_43 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (\ctx -> G     (happy_var_2 ctx)
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 (\ctx -> Union (happy_var_1 ctx) (happy_var_3 ctx)
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  22 happyReduction_45
happyReduction_45 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (\ctx -> FAtom(AEq(fst(happy_var_1 ctx))(fst(happy_var_3 ctx)))
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (\ctx -> FAtom(ALt(fst(happy_var_1 ctx))(fst(happy_var_3 ctx)))
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (\ctx -> FAtom(AGt(fst(happy_var_1 ctx))(fst(happy_var_3 ctx)))
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22 happyReduction_48
happyReduction_48 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (\ctx -> FAtom(ALe(fst(happy_var_1 ctx))(fst(happy_var_3 ctx)))
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  22 happyReduction_49
happyReduction_49 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn22
		 (\ctx -> FAtom(AGe(fst(happy_var_1 ctx))(fst(happy_var_3 ctx)))
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  22 happyReduction_50
happyReduction_50 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  23 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  23 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn23
		 (\ctx -> FTrue
	)

happyReduce_53 = happySpecReduce_1  24 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn24
		 ("=="
	)

happyReduce_54 = happySpecReduce_1  24 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn24
		 ("!="
	)

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn24
		 ("<"
	)

happyReduce_56 = happySpecReduce_1  24 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn24
		 (">"
	)

happyReduce_57 = happySpecReduce_1  24 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn24
		 ("<="
	)

happyReduce_58 = happySpecReduce_1  24 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn24
		 (">="
	)

happyReduce_59 = happySpecReduce_1  24 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn24
		 ("+"
	)

happyReduce_60 = happySpecReduce_1  24 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn24
		 ("-"
	)

happyReduce_61 = happySpecReduce_1  24 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn24
		 ("*"
	)

happyReduce_62 = happySpecReduce_1  24 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn24
		 ("/"
	)

happyReduce_63 = happySpecReduce_1  24 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn24
		 ("%"
	)

happyReduce_64 = happyReduce 6 25 happyReduction_64
happyReduction_64 ((HappyAbsSyn25  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (\ctx -> 
                                            let (b,ctx') = happy_var_2 ctx in 
                                            let (t1,ctx'') = happy_var_4 ctx' in 
                                            let (t2,ctx''') = happy_var_6 ctx'' in 
                                            (RED TmIF [b,t1,t2], ctx''')
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  25 happyReduction_65
happyReduction_65 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (\ctx -> 
                                            let (t1,ctx') = happy_var_1 ctx in 
                                            let (t2,ctx'') = happy_var_3 ctx' in 
                                            (RED (TmBOP happy_var_2) [t1,t2], ctx'')
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  25 happyReduction_66
happyReduction_66 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  26 happyReduction_67
happyReduction_67 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  26 happyReduction_68
happyReduction_68 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (\ctx -> 
                                            let (t,ctx') = happy_var_2 ctx in 
                                            (RED TmNOT [t], ctx')
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  26 happyReduction_69
happyReduction_69 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (\ctx -> 
                                            let (t,ctx') = happy_var_2 ctx in 
                                            (RED (TmUOP "-") [t], ctx')
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  26 happyReduction_70
happyReduction_70 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (\ctx -> 
                                            let (t1,ctx') = happy_var_1 ctx in 
                                            let (t2,ctx'') = happy_var_2 ctx' in 
                                            (RED TmAPP [t1,t2], ctx'')
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  27 happyReduction_71
happyReduction_71 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  28 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  28 happyReduction_73
happyReduction_73 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (\ctx -> 
                                            let (tm,ctx') = happy_var_2 ctx in 
                                            (RED TmRET [tm],ctx')
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  28 happyReduction_74
happyReduction_74 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (\ctx -> (RED happy_var_1 [], ctx)
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  28 happyReduction_75
happyReduction_75 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn28
		 (\ctx -> (RED happy_var_1 [], ctx)
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  28 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn28
		 (\ctx -> (RED TmAMOUNT [], ctx)
	)

happyReduce_77 = happySpecReduce_1  28 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn28
		 (\ctx -> (RED TmTHIS   [], ctx)
	)

happyReduce_78 = happySpecReduce_1  28 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn28
		 (\ctx -> (RED TmSENDER [], ctx)
	)

happyReduce_79 = happySpecReduce_1  28 happyReduction_79
happyReduction_79 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn28
		 (\ctx -> (RED (lookup happy_var_1 ctx) [], ctx)
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  29 happyReduction_80
happyReduction_80 (HappyTerminal (NUM happy_var_1))
	 =  HappyAbsSyn29
		 (TmU256 happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  30 happyReduction_81
happyReduction_81 _
	 =  HappyAbsSyn30
		 (TmTRUE
	)

happyReduce_82 = happySpecReduce_1  30 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn30
		 (TmFALSE
	)

happyNewToken action sts stk [] =
	action 101 101 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LET' -> cont 31;
	TRUE -> cont 32;
	FALSE -> cont 33;
	NUM happy_dollar_dollar -> cont 34;
	EVENT -> cont 35;
	CONTRACT -> cont 36;
	METHOD -> cont 37;
	RETURN -> cont 38;
	BECOME -> cont 39;
	IF -> cont 40;
	THEN -> cont 41;
	ELSE -> cont 42;
	I8 -> cont 43;
	U8 -> cont 44;
	I256 -> cont 45;
	U256 -> cont 46;
	BOOL -> cont 47;
	CASE -> cont 48;
	NEW -> cont 49;
	CALL -> cont 50;
	SENDER -> cont 51;
	SEND -> cont 52;
	MSG -> cont 53;
	AMOUNT -> cont 54;
	BALANCE -> cont 55;
	DESTRUCT -> cont 56;
	THIS -> cont 57;
	NOW -> cont 58;
	UNIT -> cont 59;
	LOG -> cont 60;
	KECCAK -> cont 61;
	ARROW -> cont 62;
	DARROW -> cont 63;
	LPAREN -> cont 64;
	RPAREN -> cont 65;
	LBRACE -> cont 66;
	RBRACE -> cont 67;
	LSQUARE -> cont 68;
	RSQUARE -> cont 69;
	SEMI -> cont 70;
	COLON -> cont 71;
	EQ -> cont 72;
	COLONEQ -> cont 73;
	COMMA -> cont 74;
	DOT -> cont 75;
	NOT -> cont 76;
	GT -> cont 77;
	LT -> cont 78;
	GE -> cont 79;
	LE -> cont 80;
	EQEQ -> cont 81;
	NEQ -> cont 82;
	PLUS -> cont 83;
	MINUS -> cont 84;
	MULT -> cont 85;
	DIV -> cont 86;
	MOD -> cont 87;
	COMMENT happy_dollar_dollar -> cont 88;
	ID happy_dollar_dollar -> cont 89;
	POSSIBLY -> cont 90;
	NECESSARILY -> cont 91;
	NEXT -> cont 92;
	FUTURELY -> cont 93;
	GLOBALLY -> cont 94;
	UNTIL -> cont 95;
	ALWAYS -> cont 96;
	NEVER -> cont 97;
	BY -> cont 98;
	AND' -> cont 99;
	OR' -> cont 100;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 101 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError t = error $ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
