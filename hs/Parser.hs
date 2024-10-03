{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer 
import Prelude hiding (lookup, lex, EQ, LT, GT) 

import GCLL
import Tree
import Type
import Data
import Term
import AST
import PsrCtx
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31
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
	| HappyAbsSyn31 t31

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,573) ([0,0,16,0,0,0,0,0,1024,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,32768,0,0,0,8,0,0,0,0,8,0,0,0,2048,0,0,32768,0,0,0,2,0,0,32,0,0,0,0,512,0,0,0,0,0,0,0,2,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,1024,0,8,0,0,63488,2048,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16448,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,248,264,0,0,0,0,0,0,32,0,0,0,0,0,16,8192,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,57344,8195,4,0,0,0,0,0,576,4,0,0,0,0,32768,0,0,0,0,32768,32783,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,8195,4,0,0,0,0,0,64,2,0,0,0,15872,16896,0,0,0,0,0,0,36,0,0,0,0,0,4096,0,0,0,0,0,16384,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,18176,18433,258,4112,270,0,0,0,0,0,0,0,0,0,0,128,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65024,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1308,2336,16388,2112,0,0,18176,18433,258,4112,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,327,584,4097,3600,1,0,4544,37376,64,32768,0,0,28672,32772,4132,0,32,0,0,0,0,0,0,0,0,18176,18433,258,4112,382,0,49152,81,16530,1024,24452,0,0,5232,9344,16,57601,16,0,7168,8197,1033,16448,8,0,32768,0,0,0,0,0,0,0,0,256,16376,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,18176,18433,258,4112,2,0,49152,17,16530,0,128,0,0,0,0,0,0,0,0,0,0,2048,65408,3,0,0,0,0,1,512,0,0,0,0,0,0,0,0,28672,32788,4132,256,33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,65408,3,0,0,0,0,0,0,0,0,0,0,0,0,544,0,0,0,0,0,0,0,0,1308,2336,16388,14400,4,0,18176,18433,258,4112,270,0,49152,81,16530,1024,17284,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,512,0,0,0,8192,65024,15,0,0,2048,0,32768,1023,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,18176,18433,258,4112,270,0,0,0,0,0,0,0,0,0,0,0,4094,0,0,0,0,0,65408,3,0,0,0,0,57344,255,0,0,0,0,0,16376,0,0,0,0,0,65024,15,0,0,1308,2336,16388,2112,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20928,37376,64,33796,67,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,49152,81,16530,1024,132,0,0,0,0,0,0,0,0,0,16,0,65408,3,0,0,327,584,4097,528,0,0,0,0,4096,16376,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","TopLevel","Contract","Top","Mthd","StoVars","Params","Param","IDs","Ty","Tys","ATy","Body","Decls","Decl","Predicate","Formulae","AppFormulae","PathFormulae","TFormulae","AFormulae","BOp","Tm","AppTm","PathTm","ATm","Numm","Num","Bool","let","true","false","num","event","contract","method","return","become","if","then","else","i8","u8","i256","u256","bool","case","new","call","sender","send","msg","amount","balance","dstrct","this","now","'()'","log","kec","'->'","'=>'","'('","')'","'{'","'}'","'['","']'","';'","':'","'='","':='","','","'.'","'~'","'>'","'<'","'>='","'<='","'=='","'!='","'+'","'-'","'*'","'/'","'%'","comment","id","E","A","X","F","G","U","always","never","by","and","or","%eof"]
        bit_start = st Prelude.* 102
        bit_end = (st Prelude.+ 1) Prelude.* 102
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..101]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (37) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (37) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyShift action_3
action_2 (4) = happyGoto action_6
action_2 (5) = happyGoto action_2
action_2 _ = happyReduce_2

action_3 (90) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (102) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (67) = happyShift action_7
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_1

action_7 (38) = happyShift action_12
action_7 (72) = happyReduce_14
action_7 (90) = happyShift action_13
action_7 (6) = happyGoto action_8
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (11) = happyGoto action_11
action_7 _ = happyReduce_6

action_8 (68) = happyShift action_19
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (38) = happyShift action_12
action_9 (72) = happyReduce_14
action_9 (90) = happyShift action_13
action_9 (6) = happyGoto action_18
action_9 (7) = happyGoto action_9
action_9 (8) = happyGoto action_10
action_9 (11) = happyGoto action_11
action_9 _ = happyReduce_6

action_10 (38) = happyShift action_12
action_10 (72) = happyReduce_14
action_10 (90) = happyShift action_13
action_10 (6) = happyGoto action_17
action_10 (7) = happyGoto action_9
action_10 (8) = happyGoto action_10
action_10 (11) = happyGoto action_11
action_10 _ = happyReduce_6

action_11 (72) = happyShift action_16
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (90) = happyShift action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (90) = happyShift action_13
action_13 (11) = happyGoto action_14
action_13 _ = happyReduce_14

action_14 _ = happyReduce_13

action_15 (65) = happyShift action_31
action_15 (90) = happyShift action_32
action_15 (9) = happyGoto action_29
action_15 (10) = happyGoto action_30
action_15 _ = happyReduce_10

action_16 (44) = happyShift action_22
action_16 (45) = happyShift action_23
action_16 (46) = happyShift action_24
action_16 (47) = happyShift action_25
action_16 (48) = happyShift action_26
action_16 (60) = happyShift action_27
action_16 (65) = happyShift action_28
action_16 (12) = happyGoto action_20
action_16 (14) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_4

action_18 _ = happyReduce_5

action_19 _ = happyReduce_3

action_20 (63) = happyShift action_38
action_20 (71) = happyShift action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_15

action_22 _ = happyReduce_22

action_23 _ = happyReduce_21

action_24 _ = happyReduce_24

action_25 _ = happyReduce_23

action_26 _ = happyReduce_20

action_27 _ = happyReduce_25

action_28 (44) = happyShift action_22
action_28 (45) = happyShift action_23
action_28 (46) = happyShift action_24
action_28 (47) = happyShift action_25
action_28 (48) = happyShift action_26
action_28 (60) = happyShift action_27
action_28 (65) = happyShift action_28
action_28 (12) = happyGoto action_36
action_28 (13) = happyGoto action_37
action_28 (14) = happyGoto action_21
action_28 _ = happyReduce_19

action_29 (72) = happyShift action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (65) = happyShift action_31
action_30 (90) = happyShift action_32
action_30 (9) = happyGoto action_34
action_30 (10) = happyGoto action_30
action_30 _ = happyReduce_10

action_31 (90) = happyShift action_13
action_31 (11) = happyGoto action_33
action_31 _ = happyReduce_14

action_32 _ = happyReduce_12

action_33 (72) = happyShift action_45
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_9

action_35 (44) = happyShift action_22
action_35 (45) = happyShift action_23
action_35 (46) = happyShift action_24
action_35 (47) = happyShift action_25
action_35 (48) = happyShift action_26
action_35 (60) = happyShift action_27
action_35 (65) = happyShift action_28
action_35 (12) = happyGoto action_44
action_35 (14) = happyGoto action_21
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (63) = happyShift action_38
action_36 (66) = happyShift action_42
action_36 (75) = happyShift action_43
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (66) = happyShift action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (44) = happyShift action_22
action_38 (45) = happyShift action_23
action_38 (46) = happyShift action_24
action_38 (47) = happyShift action_25
action_38 (48) = happyShift action_26
action_38 (60) = happyShift action_27
action_38 (65) = happyShift action_28
action_38 (12) = happyGoto action_40
action_38 (14) = happyGoto action_21
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_8

action_40 (63) = happyShift action_38
action_40 _ = happyReduce_16

action_41 _ = happyReduce_17

action_42 _ = happyReduce_26

action_43 (44) = happyShift action_22
action_43 (45) = happyShift action_23
action_43 (46) = happyShift action_24
action_43 (47) = happyShift action_25
action_43 (48) = happyShift action_26
action_43 (60) = happyShift action_27
action_43 (65) = happyShift action_28
action_43 (12) = happyGoto action_48
action_43 (13) = happyGoto action_49
action_43 (14) = happyGoto action_21
action_43 _ = happyReduce_19

action_44 (63) = happyShift action_38
action_44 (74) = happyShift action_47
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (44) = happyShift action_22
action_45 (45) = happyShift action_23
action_45 (46) = happyShift action_24
action_45 (47) = happyShift action_25
action_45 (48) = happyShift action_26
action_45 (60) = happyShift action_27
action_45 (65) = happyShift action_28
action_45 (12) = happyGoto action_46
action_45 (14) = happyGoto action_21
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (63) = happyShift action_38
action_46 (66) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (67) = happyShift action_52
action_47 (15) = happyGoto action_50
action_47 (18) = happyGoto action_51
action_47 _ = happyReduce_33

action_48 (63) = happyShift action_38
action_48 (75) = happyShift action_43
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_18

action_50 _ = happyReduce_7

action_51 (32) = happyShift action_82
action_51 (33) = happyShift action_83
action_51 (34) = happyShift action_65
action_51 (35) = happyShift action_66
action_51 (39) = happyShift action_67
action_51 (41) = happyShift action_68
action_51 (52) = happyShift action_69
action_51 (55) = happyShift action_70
action_51 (58) = happyShift action_71
action_51 (65) = happyShift action_84
action_51 (77) = happyShift action_85
action_51 (85) = happyShift action_74
action_51 (90) = happyShift action_75
action_51 (16) = happyGoto action_79
action_51 (17) = happyGoto action_80
action_51 (25) = happyGoto action_81
action_51 (26) = happyGoto action_59
action_51 (27) = happyGoto action_60
action_51 (28) = happyGoto action_61
action_51 (30) = happyGoto action_62
action_51 (31) = happyGoto action_63
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (33) = happyShift action_64
action_52 (34) = happyShift action_65
action_52 (35) = happyShift action_66
action_52 (39) = happyShift action_67
action_52 (41) = happyShift action_68
action_52 (52) = happyShift action_69
action_52 (55) = happyShift action_70
action_52 (58) = happyShift action_71
action_52 (65) = happyShift action_72
action_52 (77) = happyShift action_73
action_52 (85) = happyShift action_74
action_52 (90) = happyShift action_75
action_52 (91) = happyShift action_76
action_52 (92) = happyShift action_77
action_52 (97) = happyShift action_78
action_52 (19) = happyGoto action_54
action_52 (20) = happyGoto action_55
action_52 (22) = happyGoto action_56
action_52 (23) = happyGoto action_57
action_52 (25) = happyGoto action_58
action_52 (26) = happyGoto action_59
action_52 (27) = happyGoto action_60
action_52 (28) = happyGoto action_61
action_52 (30) = happyGoto action_62
action_52 (31) = happyGoto action_63
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_11

action_54 (68) = happyShift action_123
action_54 (100) = happyShift action_124
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_38

action_56 _ = happyReduce_40

action_57 _ = happyReduce_50

action_58 (78) = happyShift action_118
action_58 (79) = happyShift action_119
action_58 (80) = happyShift action_120
action_58 (81) = happyShift action_121
action_58 (82) = happyShift action_122
action_58 (83) = happyShift action_96
action_58 (84) = happyShift action_97
action_58 (85) = happyShift action_98
action_58 (86) = happyShift action_99
action_58 (87) = happyShift action_100
action_58 (88) = happyShift action_101
action_58 (24) = happyGoto action_90
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (33) = happyShift action_83
action_59 (34) = happyShift action_65
action_59 (35) = happyShift action_66
action_59 (39) = happyShift action_67
action_59 (52) = happyShift action_69
action_59 (55) = happyShift action_70
action_59 (58) = happyShift action_71
action_59 (65) = happyShift action_84
action_59 (90) = happyShift action_75
action_59 (27) = happyGoto action_117
action_59 (28) = happyGoto action_61
action_59 (30) = happyGoto action_62
action_59 (31) = happyGoto action_63
action_59 _ = happyReduce_66

action_60 _ = happyReduce_67

action_61 _ = happyReduce_71

action_62 _ = happyReduce_74

action_63 _ = happyReduce_75

action_64 (66) = happyReduce_82
action_64 (68) = happyReduce_52
action_64 (96) = happyReduce_52
action_64 (100) = happyReduce_52
action_64 _ = happyReduce_82

action_65 _ = happyReduce_83

action_66 _ = happyReduce_81

action_67 (33) = happyShift action_83
action_67 (34) = happyShift action_65
action_67 (35) = happyShift action_66
action_67 (39) = happyShift action_67
action_67 (41) = happyShift action_68
action_67 (52) = happyShift action_69
action_67 (55) = happyShift action_70
action_67 (58) = happyShift action_71
action_67 (65) = happyShift action_84
action_67 (77) = happyShift action_85
action_67 (85) = happyShift action_74
action_67 (90) = happyShift action_75
action_67 (25) = happyGoto action_116
action_67 (26) = happyGoto action_59
action_67 (27) = happyGoto action_60
action_67 (28) = happyGoto action_61
action_67 (30) = happyGoto action_62
action_67 (31) = happyGoto action_63
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (33) = happyShift action_83
action_68 (34) = happyShift action_65
action_68 (35) = happyShift action_66
action_68 (39) = happyShift action_67
action_68 (41) = happyShift action_68
action_68 (52) = happyShift action_69
action_68 (55) = happyShift action_70
action_68 (58) = happyShift action_71
action_68 (65) = happyShift action_84
action_68 (77) = happyShift action_85
action_68 (85) = happyShift action_74
action_68 (90) = happyShift action_75
action_68 (25) = happyGoto action_115
action_68 (26) = happyGoto action_59
action_68 (27) = happyGoto action_60
action_68 (28) = happyGoto action_61
action_68 (30) = happyGoto action_62
action_68 (31) = happyGoto action_63
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_78

action_70 _ = happyReduce_76

action_71 _ = happyReduce_77

action_72 (33) = happyShift action_64
action_72 (34) = happyShift action_65
action_72 (35) = happyShift action_66
action_72 (39) = happyShift action_67
action_72 (41) = happyShift action_68
action_72 (52) = happyShift action_69
action_72 (55) = happyShift action_70
action_72 (58) = happyShift action_71
action_72 (65) = happyShift action_72
action_72 (77) = happyShift action_73
action_72 (85) = happyShift action_74
action_72 (90) = happyShift action_75
action_72 (91) = happyShift action_76
action_72 (92) = happyShift action_77
action_72 (97) = happyShift action_78
action_72 (19) = happyGoto action_113
action_72 (20) = happyGoto action_55
action_72 (22) = happyGoto action_56
action_72 (23) = happyGoto action_57
action_72 (25) = happyGoto action_114
action_72 (26) = happyGoto action_59
action_72 (27) = happyGoto action_60
action_72 (28) = happyGoto action_61
action_72 (30) = happyGoto action_62
action_72 (31) = happyGoto action_63
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (33) = happyShift action_64
action_73 (34) = happyShift action_65
action_73 (35) = happyShift action_66
action_73 (39) = happyShift action_67
action_73 (52) = happyShift action_69
action_73 (55) = happyShift action_70
action_73 (58) = happyShift action_71
action_73 (65) = happyShift action_72
action_73 (90) = happyShift action_75
action_73 (23) = happyGoto action_112
action_73 (27) = happyGoto action_86
action_73 (28) = happyGoto action_61
action_73 (30) = happyGoto action_62
action_73 (31) = happyGoto action_63
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (33) = happyShift action_83
action_74 (34) = happyShift action_65
action_74 (35) = happyShift action_66
action_74 (39) = happyShift action_67
action_74 (52) = happyShift action_69
action_74 (55) = happyShift action_70
action_74 (58) = happyShift action_71
action_74 (65) = happyShift action_84
action_74 (90) = happyShift action_75
action_74 (27) = happyGoto action_111
action_74 (28) = happyGoto action_61
action_74 (30) = happyGoto action_62
action_74 (31) = happyGoto action_63
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_79

action_76 (33) = happyShift action_64
action_76 (34) = happyShift action_65
action_76 (35) = happyShift action_66
action_76 (39) = happyShift action_67
action_76 (41) = happyShift action_68
action_76 (52) = happyShift action_69
action_76 (55) = happyShift action_70
action_76 (58) = happyShift action_71
action_76 (65) = happyShift action_72
action_76 (77) = happyShift action_73
action_76 (85) = happyShift action_74
action_76 (90) = happyShift action_75
action_76 (91) = happyShift action_76
action_76 (92) = happyShift action_77
action_76 (93) = happyShift action_107
action_76 (94) = happyShift action_108
action_76 (95) = happyShift action_109
action_76 (97) = happyShift action_78
action_76 (19) = happyGoto action_105
action_76 (20) = happyGoto action_55
action_76 (21) = happyGoto action_110
action_76 (22) = happyGoto action_56
action_76 (23) = happyGoto action_57
action_76 (25) = happyGoto action_58
action_76 (26) = happyGoto action_59
action_76 (27) = happyGoto action_60
action_76 (28) = happyGoto action_61
action_76 (30) = happyGoto action_62
action_76 (31) = happyGoto action_63
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (33) = happyShift action_64
action_77 (34) = happyShift action_65
action_77 (35) = happyShift action_66
action_77 (39) = happyShift action_67
action_77 (41) = happyShift action_68
action_77 (52) = happyShift action_69
action_77 (55) = happyShift action_70
action_77 (58) = happyShift action_71
action_77 (65) = happyShift action_72
action_77 (77) = happyShift action_73
action_77 (85) = happyShift action_74
action_77 (90) = happyShift action_75
action_77 (91) = happyShift action_76
action_77 (92) = happyShift action_77
action_77 (93) = happyShift action_107
action_77 (94) = happyShift action_108
action_77 (95) = happyShift action_109
action_77 (97) = happyShift action_78
action_77 (19) = happyGoto action_105
action_77 (20) = happyGoto action_55
action_77 (21) = happyGoto action_106
action_77 (22) = happyGoto action_56
action_77 (23) = happyGoto action_57
action_77 (25) = happyGoto action_58
action_77 (26) = happyGoto action_59
action_77 (27) = happyGoto action_60
action_77 (28) = happyGoto action_61
action_77 (30) = happyGoto action_62
action_77 (31) = happyGoto action_63
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (33) = happyShift action_64
action_78 (34) = happyShift action_65
action_78 (35) = happyShift action_66
action_78 (39) = happyShift action_67
action_78 (41) = happyShift action_68
action_78 (52) = happyShift action_69
action_78 (55) = happyShift action_70
action_78 (58) = happyShift action_71
action_78 (65) = happyShift action_72
action_78 (77) = happyShift action_73
action_78 (85) = happyShift action_74
action_78 (90) = happyShift action_75
action_78 (91) = happyShift action_76
action_78 (92) = happyShift action_77
action_78 (97) = happyShift action_78
action_78 (19) = happyGoto action_104
action_78 (20) = happyGoto action_55
action_78 (22) = happyGoto action_56
action_78 (23) = happyGoto action_57
action_78 (25) = happyGoto action_58
action_78 (26) = happyGoto action_59
action_78 (27) = happyGoto action_60
action_78 (28) = happyGoto action_61
action_78 (30) = happyGoto action_62
action_78 (31) = happyGoto action_63
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (33) = happyShift action_83
action_79 (34) = happyShift action_65
action_79 (35) = happyShift action_66
action_79 (39) = happyShift action_67
action_79 (41) = happyShift action_68
action_79 (52) = happyShift action_69
action_79 (55) = happyShift action_70
action_79 (58) = happyShift action_71
action_79 (65) = happyShift action_84
action_79 (77) = happyShift action_85
action_79 (85) = happyShift action_74
action_79 (90) = happyShift action_75
action_79 (25) = happyGoto action_103
action_79 (26) = happyGoto action_59
action_79 (27) = happyGoto action_60
action_79 (28) = happyGoto action_61
action_79 (30) = happyGoto action_62
action_79 (31) = happyGoto action_63
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (32) = happyShift action_82
action_80 (16) = happyGoto action_102
action_80 (17) = happyGoto action_80
action_80 _ = happyReduce_30

action_81 (67) = happyShift action_52
action_81 (78) = happyShift action_91
action_81 (79) = happyShift action_92
action_81 (80) = happyShift action_93
action_81 (81) = happyShift action_94
action_81 (82) = happyShift action_95
action_81 (83) = happyShift action_96
action_81 (84) = happyShift action_97
action_81 (85) = happyShift action_98
action_81 (86) = happyShift action_99
action_81 (87) = happyShift action_100
action_81 (88) = happyShift action_101
action_81 (18) = happyGoto action_89
action_81 (24) = happyGoto action_90
action_81 _ = happyReduce_33

action_82 (90) = happyShift action_88
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_82

action_84 (33) = happyShift action_83
action_84 (34) = happyShift action_65
action_84 (35) = happyShift action_66
action_84 (39) = happyShift action_67
action_84 (41) = happyShift action_68
action_84 (52) = happyShift action_69
action_84 (55) = happyShift action_70
action_84 (58) = happyShift action_71
action_84 (65) = happyShift action_84
action_84 (77) = happyShift action_85
action_84 (85) = happyShift action_74
action_84 (90) = happyShift action_75
action_84 (25) = happyGoto action_87
action_84 (26) = happyGoto action_59
action_84 (27) = happyGoto action_60
action_84 (28) = happyGoto action_61
action_84 (30) = happyGoto action_62
action_84 (31) = happyGoto action_63
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (33) = happyShift action_83
action_85 (34) = happyShift action_65
action_85 (35) = happyShift action_66
action_85 (39) = happyShift action_67
action_85 (52) = happyShift action_69
action_85 (55) = happyShift action_70
action_85 (58) = happyShift action_71
action_85 (65) = happyShift action_84
action_85 (90) = happyShift action_75
action_85 (27) = happyGoto action_86
action_85 (28) = happyGoto action_61
action_85 (30) = happyGoto action_62
action_85 (31) = happyGoto action_63
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_68

action_87 (66) = happyShift action_132
action_87 (78) = happyShift action_91
action_87 (79) = happyShift action_92
action_87 (80) = happyShift action_93
action_87 (81) = happyShift action_94
action_87 (82) = happyShift action_95
action_87 (83) = happyShift action_96
action_87 (84) = happyShift action_97
action_87 (85) = happyShift action_98
action_87 (86) = happyShift action_99
action_87 (87) = happyShift action_100
action_87 (88) = happyShift action_101
action_87 (24) = happyGoto action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (65) = happyShift action_31
action_88 (90) = happyShift action_32
action_88 (9) = happyGoto action_140
action_88 (10) = happyGoto action_30
action_88 _ = happyReduce_10

action_89 _ = happyReduce_27

action_90 (33) = happyShift action_83
action_90 (34) = happyShift action_65
action_90 (35) = happyShift action_66
action_90 (39) = happyShift action_67
action_90 (41) = happyShift action_68
action_90 (52) = happyShift action_69
action_90 (55) = happyShift action_70
action_90 (58) = happyShift action_71
action_90 (65) = happyShift action_84
action_90 (77) = happyShift action_85
action_90 (85) = happyShift action_74
action_90 (90) = happyShift action_75
action_90 (25) = happyGoto action_139
action_90 (26) = happyGoto action_59
action_90 (27) = happyGoto action_60
action_90 (28) = happyGoto action_61
action_90 (30) = happyGoto action_62
action_90 (31) = happyGoto action_63
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

action_103 (67) = happyShift action_52
action_103 (78) = happyShift action_91
action_103 (79) = happyShift action_92
action_103 (80) = happyShift action_93
action_103 (81) = happyShift action_94
action_103 (82) = happyShift action_95
action_103 (83) = happyShift action_96
action_103 (84) = happyShift action_97
action_103 (85) = happyShift action_98
action_103 (86) = happyShift action_99
action_103 (87) = happyShift action_100
action_103 (88) = happyShift action_101
action_103 (18) = happyGoto action_138
action_103 (24) = happyGoto action_90
action_103 _ = happyReduce_33

action_104 (100) = happyShift action_124
action_104 _ = happyReduce_37

action_105 (96) = happyShift action_137
action_105 (100) = happyShift action_124
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_36

action_107 (33) = happyShift action_64
action_107 (34) = happyShift action_65
action_107 (35) = happyShift action_66
action_107 (39) = happyShift action_67
action_107 (41) = happyShift action_68
action_107 (52) = happyShift action_69
action_107 (55) = happyShift action_70
action_107 (58) = happyShift action_71
action_107 (65) = happyShift action_72
action_107 (77) = happyShift action_73
action_107 (85) = happyShift action_74
action_107 (90) = happyShift action_75
action_107 (91) = happyShift action_76
action_107 (92) = happyShift action_77
action_107 (97) = happyShift action_78
action_107 (19) = happyGoto action_136
action_107 (20) = happyGoto action_55
action_107 (22) = happyGoto action_56
action_107 (23) = happyGoto action_57
action_107 (25) = happyGoto action_58
action_107 (26) = happyGoto action_59
action_107 (27) = happyGoto action_60
action_107 (28) = happyGoto action_61
action_107 (30) = happyGoto action_62
action_107 (31) = happyGoto action_63
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (33) = happyShift action_64
action_108 (34) = happyShift action_65
action_108 (35) = happyShift action_66
action_108 (39) = happyShift action_67
action_108 (41) = happyShift action_68
action_108 (52) = happyShift action_69
action_108 (55) = happyShift action_70
action_108 (58) = happyShift action_71
action_108 (65) = happyShift action_72
action_108 (77) = happyShift action_73
action_108 (85) = happyShift action_74
action_108 (90) = happyShift action_75
action_108 (91) = happyShift action_76
action_108 (92) = happyShift action_77
action_108 (97) = happyShift action_78
action_108 (19) = happyGoto action_135
action_108 (20) = happyGoto action_55
action_108 (22) = happyGoto action_56
action_108 (23) = happyGoto action_57
action_108 (25) = happyGoto action_58
action_108 (26) = happyGoto action_59
action_108 (27) = happyGoto action_60
action_108 (28) = happyGoto action_61
action_108 (30) = happyGoto action_62
action_108 (31) = happyGoto action_63
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (33) = happyShift action_64
action_109 (34) = happyShift action_65
action_109 (35) = happyShift action_66
action_109 (39) = happyShift action_67
action_109 (41) = happyShift action_68
action_109 (52) = happyShift action_69
action_109 (55) = happyShift action_70
action_109 (58) = happyShift action_71
action_109 (65) = happyShift action_72
action_109 (77) = happyShift action_73
action_109 (85) = happyShift action_74
action_109 (90) = happyShift action_75
action_109 (91) = happyShift action_76
action_109 (92) = happyShift action_77
action_109 (97) = happyShift action_78
action_109 (19) = happyGoto action_134
action_109 (20) = happyGoto action_55
action_109 (22) = happyGoto action_56
action_109 (23) = happyGoto action_57
action_109 (25) = happyGoto action_58
action_109 (26) = happyGoto action_59
action_109 (27) = happyGoto action_60
action_109 (28) = happyGoto action_61
action_109 (30) = happyGoto action_62
action_109 (31) = happyGoto action_63
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_35

action_111 _ = happyReduce_69

action_112 _ = happyReduce_34

action_113 (66) = happyShift action_133
action_113 (100) = happyShift action_124
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (66) = happyShift action_132
action_114 (78) = happyShift action_118
action_114 (79) = happyShift action_119
action_114 (80) = happyShift action_120
action_114 (81) = happyShift action_121
action_114 (82) = happyShift action_122
action_114 (83) = happyShift action_96
action_114 (84) = happyShift action_97
action_114 (85) = happyShift action_98
action_114 (86) = happyShift action_99
action_114 (87) = happyShift action_100
action_114 (88) = happyShift action_101
action_114 (24) = happyGoto action_90
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (42) = happyShift action_131
action_115 (78) = happyShift action_91
action_115 (79) = happyShift action_92
action_115 (80) = happyShift action_93
action_115 (81) = happyShift action_94
action_115 (82) = happyShift action_95
action_115 (83) = happyShift action_96
action_115 (84) = happyShift action_97
action_115 (85) = happyShift action_98
action_115 (86) = happyShift action_99
action_115 (87) = happyShift action_100
action_115 (88) = happyShift action_101
action_115 (24) = happyGoto action_90
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (78) = happyShift action_91
action_116 (79) = happyShift action_92
action_116 (80) = happyShift action_93
action_116 (81) = happyShift action_94
action_116 (82) = happyShift action_95
action_116 (83) = happyShift action_96
action_116 (84) = happyShift action_97
action_116 (85) = happyShift action_98
action_116 (86) = happyShift action_99
action_116 (87) = happyShift action_100
action_116 (88) = happyShift action_101
action_116 (24) = happyGoto action_90
action_116 _ = happyReduce_73

action_117 _ = happyReduce_70

action_118 (33) = happyShift action_83
action_118 (34) = happyShift action_65
action_118 (35) = happyShift action_66
action_118 (39) = happyShift action_67
action_118 (41) = happyShift action_68
action_118 (52) = happyShift action_69
action_118 (55) = happyShift action_70
action_118 (58) = happyShift action_71
action_118 (65) = happyShift action_84
action_118 (77) = happyShift action_85
action_118 (85) = happyShift action_74
action_118 (90) = happyShift action_75
action_118 (25) = happyGoto action_130
action_118 (26) = happyGoto action_59
action_118 (27) = happyGoto action_60
action_118 (28) = happyGoto action_61
action_118 (30) = happyGoto action_62
action_118 (31) = happyGoto action_63
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (33) = happyShift action_83
action_119 (34) = happyShift action_65
action_119 (35) = happyShift action_66
action_119 (39) = happyShift action_67
action_119 (41) = happyShift action_68
action_119 (52) = happyShift action_69
action_119 (55) = happyShift action_70
action_119 (58) = happyShift action_71
action_119 (65) = happyShift action_84
action_119 (77) = happyShift action_85
action_119 (85) = happyShift action_74
action_119 (90) = happyShift action_75
action_119 (25) = happyGoto action_129
action_119 (26) = happyGoto action_59
action_119 (27) = happyGoto action_60
action_119 (28) = happyGoto action_61
action_119 (30) = happyGoto action_62
action_119 (31) = happyGoto action_63
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (33) = happyShift action_83
action_120 (34) = happyShift action_65
action_120 (35) = happyShift action_66
action_120 (39) = happyShift action_67
action_120 (41) = happyShift action_68
action_120 (52) = happyShift action_69
action_120 (55) = happyShift action_70
action_120 (58) = happyShift action_71
action_120 (65) = happyShift action_84
action_120 (77) = happyShift action_85
action_120 (85) = happyShift action_74
action_120 (90) = happyShift action_75
action_120 (25) = happyGoto action_128
action_120 (26) = happyGoto action_59
action_120 (27) = happyGoto action_60
action_120 (28) = happyGoto action_61
action_120 (30) = happyGoto action_62
action_120 (31) = happyGoto action_63
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (33) = happyShift action_83
action_121 (34) = happyShift action_65
action_121 (35) = happyShift action_66
action_121 (39) = happyShift action_67
action_121 (41) = happyShift action_68
action_121 (52) = happyShift action_69
action_121 (55) = happyShift action_70
action_121 (58) = happyShift action_71
action_121 (65) = happyShift action_84
action_121 (77) = happyShift action_85
action_121 (85) = happyShift action_74
action_121 (90) = happyShift action_75
action_121 (25) = happyGoto action_127
action_121 (26) = happyGoto action_59
action_121 (27) = happyGoto action_60
action_121 (28) = happyGoto action_61
action_121 (30) = happyGoto action_62
action_121 (31) = happyGoto action_63
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (33) = happyShift action_83
action_122 (34) = happyShift action_65
action_122 (35) = happyShift action_66
action_122 (39) = happyShift action_67
action_122 (41) = happyShift action_68
action_122 (52) = happyShift action_69
action_122 (55) = happyShift action_70
action_122 (58) = happyShift action_71
action_122 (65) = happyShift action_84
action_122 (77) = happyShift action_85
action_122 (85) = happyShift action_74
action_122 (90) = happyShift action_75
action_122 (25) = happyGoto action_126
action_122 (26) = happyGoto action_59
action_122 (27) = happyGoto action_60
action_122 (28) = happyGoto action_61
action_122 (30) = happyGoto action_62
action_122 (31) = happyGoto action_63
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_32

action_124 (33) = happyShift action_64
action_124 (34) = happyShift action_65
action_124 (35) = happyShift action_66
action_124 (39) = happyShift action_67
action_124 (41) = happyShift action_68
action_124 (52) = happyShift action_69
action_124 (55) = happyShift action_70
action_124 (58) = happyShift action_71
action_124 (65) = happyShift action_72
action_124 (77) = happyShift action_73
action_124 (85) = happyShift action_74
action_124 (90) = happyShift action_75
action_124 (91) = happyShift action_76
action_124 (92) = happyShift action_77
action_124 (97) = happyShift action_78
action_124 (19) = happyGoto action_125
action_124 (20) = happyGoto action_55
action_124 (22) = happyGoto action_56
action_124 (23) = happyGoto action_57
action_124 (25) = happyGoto action_58
action_124 (26) = happyGoto action_59
action_124 (27) = happyGoto action_60
action_124 (28) = happyGoto action_61
action_124 (30) = happyGoto action_62
action_124 (31) = happyGoto action_63
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (100) = happyShift action_124
action_125 _ = happyReduce_39

action_126 (78) = happyShift action_91
action_126 (79) = happyShift action_92
action_126 (80) = happyShift action_93
action_126 (81) = happyShift action_94
action_126 (82) = happyShift action_95
action_126 (83) = happyShift action_96
action_126 (84) = happyShift action_97
action_126 (85) = happyShift action_98
action_126 (86) = happyShift action_99
action_126 (87) = happyShift action_100
action_126 (88) = happyShift action_101
action_126 (24) = happyGoto action_90
action_126 _ = happyReduce_45

action_127 (78) = happyShift action_91
action_127 (79) = happyShift action_92
action_127 (80) = happyShift action_93
action_127 (81) = happyShift action_94
action_127 (82) = happyShift action_95
action_127 (83) = happyShift action_96
action_127 (84) = happyShift action_97
action_127 (85) = happyShift action_98
action_127 (86) = happyShift action_99
action_127 (87) = happyShift action_100
action_127 (88) = happyShift action_101
action_127 (24) = happyGoto action_90
action_127 _ = happyReduce_48

action_128 (78) = happyShift action_91
action_128 (79) = happyShift action_92
action_128 (80) = happyShift action_93
action_128 (81) = happyShift action_94
action_128 (82) = happyShift action_95
action_128 (83) = happyShift action_96
action_128 (84) = happyShift action_97
action_128 (85) = happyShift action_98
action_128 (86) = happyShift action_99
action_128 (87) = happyShift action_100
action_128 (88) = happyShift action_101
action_128 (24) = happyGoto action_90
action_128 _ = happyReduce_49

action_129 (78) = happyShift action_91
action_129 (79) = happyShift action_92
action_129 (80) = happyShift action_93
action_129 (81) = happyShift action_94
action_129 (82) = happyShift action_95
action_129 (83) = happyShift action_96
action_129 (84) = happyShift action_97
action_129 (85) = happyShift action_98
action_129 (86) = happyShift action_99
action_129 (87) = happyShift action_100
action_129 (88) = happyShift action_101
action_129 (24) = happyGoto action_90
action_129 _ = happyReduce_46

action_130 (78) = happyShift action_91
action_130 (79) = happyShift action_92
action_130 (80) = happyShift action_93
action_130 (81) = happyShift action_94
action_130 (82) = happyShift action_95
action_130 (83) = happyShift action_96
action_130 (84) = happyShift action_97
action_130 (85) = happyShift action_98
action_130 (86) = happyShift action_99
action_130 (87) = happyShift action_100
action_130 (88) = happyShift action_101
action_130 (24) = happyGoto action_90
action_130 _ = happyReduce_47

action_131 (33) = happyShift action_83
action_131 (34) = happyShift action_65
action_131 (35) = happyShift action_66
action_131 (39) = happyShift action_67
action_131 (41) = happyShift action_68
action_131 (52) = happyShift action_69
action_131 (55) = happyShift action_70
action_131 (58) = happyShift action_71
action_131 (65) = happyShift action_84
action_131 (77) = happyShift action_85
action_131 (85) = happyShift action_74
action_131 (90) = happyShift action_75
action_131 (25) = happyGoto action_143
action_131 (26) = happyGoto action_59
action_131 (27) = happyGoto action_60
action_131 (28) = happyGoto action_61
action_131 (30) = happyGoto action_62
action_131 (31) = happyGoto action_63
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_72

action_133 _ = happyReduce_51

action_134 (100) = happyShift action_124
action_134 _ = happyReduce_43

action_135 (100) = happyShift action_124
action_135 _ = happyReduce_42

action_136 (100) = happyShift action_124
action_136 _ = happyReduce_41

action_137 (33) = happyShift action_64
action_137 (34) = happyShift action_65
action_137 (35) = happyShift action_66
action_137 (39) = happyShift action_67
action_137 (41) = happyShift action_68
action_137 (52) = happyShift action_69
action_137 (55) = happyShift action_70
action_137 (58) = happyShift action_71
action_137 (65) = happyShift action_72
action_137 (77) = happyShift action_73
action_137 (85) = happyShift action_74
action_137 (90) = happyShift action_75
action_137 (91) = happyShift action_76
action_137 (92) = happyShift action_77
action_137 (97) = happyShift action_78
action_137 (19) = happyGoto action_142
action_137 (20) = happyGoto action_55
action_137 (22) = happyGoto action_56
action_137 (23) = happyGoto action_57
action_137 (25) = happyGoto action_58
action_137 (26) = happyGoto action_59
action_137 (27) = happyGoto action_60
action_137 (28) = happyGoto action_61
action_137 (30) = happyGoto action_62
action_137 (31) = happyGoto action_63
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_28

action_139 (78) = happyShift action_91
action_139 (79) = happyShift action_92
action_139 (80) = happyShift action_93
action_139 (81) = happyShift action_94
action_139 (82) = happyShift action_95
action_139 (83) = happyShift action_96
action_139 (84) = happyShift action_97
action_139 (85) = happyShift action_98
action_139 (86) = happyShift action_99
action_139 (87) = happyShift action_100
action_139 (88) = happyShift action_101
action_139 (24) = happyGoto action_90
action_139 _ = happyReduce_65

action_140 (73) = happyShift action_141
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (33) = happyShift action_83
action_141 (34) = happyShift action_65
action_141 (35) = happyShift action_66
action_141 (39) = happyShift action_67
action_141 (41) = happyShift action_68
action_141 (52) = happyShift action_69
action_141 (55) = happyShift action_70
action_141 (58) = happyShift action_71
action_141 (65) = happyShift action_84
action_141 (77) = happyShift action_85
action_141 (85) = happyShift action_74
action_141 (90) = happyShift action_75
action_141 (25) = happyGoto action_145
action_141 (26) = happyGoto action_59
action_141 (27) = happyGoto action_60
action_141 (28) = happyGoto action_61
action_141 (30) = happyGoto action_62
action_141 (31) = happyGoto action_63
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (100) = happyShift action_124
action_142 _ = happyReduce_44

action_143 (43) = happyShift action_144
action_143 (78) = happyShift action_91
action_143 (79) = happyShift action_92
action_143 (80) = happyShift action_93
action_143 (81) = happyShift action_94
action_143 (82) = happyShift action_95
action_143 (83) = happyShift action_96
action_143 (84) = happyShift action_97
action_143 (85) = happyShift action_98
action_143 (86) = happyShift action_99
action_143 (87) = happyShift action_100
action_143 (88) = happyShift action_101
action_143 (24) = happyGoto action_90
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (33) = happyShift action_83
action_144 (34) = happyShift action_65
action_144 (35) = happyShift action_66
action_144 (39) = happyShift action_67
action_144 (41) = happyShift action_68
action_144 (52) = happyShift action_69
action_144 (55) = happyShift action_70
action_144 (58) = happyShift action_71
action_144 (65) = happyShift action_84
action_144 (77) = happyShift action_85
action_144 (85) = happyShift action_74
action_144 (90) = happyShift action_75
action_144 (25) = happyGoto action_147
action_144 (26) = happyGoto action_59
action_144 (27) = happyGoto action_60
action_144 (28) = happyGoto action_61
action_144 (30) = happyGoto action_62
action_144 (31) = happyGoto action_63
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (71) = happyShift action_146
action_145 (78) = happyShift action_91
action_145 (79) = happyShift action_92
action_145 (80) = happyShift action_93
action_145 (81) = happyShift action_94
action_145 (82) = happyShift action_95
action_145 (83) = happyShift action_96
action_145 (84) = happyShift action_97
action_145 (85) = happyShift action_98
action_145 (86) = happyShift action_99
action_145 (87) = happyShift action_100
action_145 (88) = happyShift action_101
action_145 (24) = happyGoto action_90
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (67) = happyShift action_52
action_146 (18) = happyGoto action_148
action_146 _ = happyReduce_33

action_147 (78) = happyShift action_91
action_147 (79) = happyShift action_92
action_147 (80) = happyShift action_93
action_147 (81) = happyShift action_94
action_147 (82) = happyShift action_95
action_147 (83) = happyShift action_96
action_147 (84) = happyShift action_97
action_147 (85) = happyShift action_98
action_147 (86) = happyShift action_99
action_147 (87) = happyShift action_100
action_147 (88) = happyShift action_101
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
                                                let id'             = '\'':happy_var_2 in 
                                                let ctx'            = addCtx STO happy_var_2 ctx     in 
                                                let ctx''''         = addCtx STO id' ctx    in 
                                                let ([], ctx'')     = happy_var_3 ctx'               in 
                                                let (tm, ctx''')    = happy_var_5 ctx''''            in 
                                                (SLET id' tm (happy_var_7 ctx'''), ctx')
                                            _               -> 
                                                let ctx'            = addCtx VAR happy_var_2 ctx     in 
                                                let (params, ctx'') = happy_var_3 ctx'               in 
                                                case params of 
                                                []          -> 
                                                    let (tm, ctx''')    = happy_var_5 ctx''              in 
                                                    (LET  happy_var_2 tm (happy_var_7 ctx'''), ctx') 
                                                _           -> 
                                                    let (tm, ctx''')    = happy_var_5 ctx''              in 
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
		 (\ctx -> FE    (happy_var_2 ctx)
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  19 happyReduction_36
happyReduction_36 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> FA    (happy_var_2 ctx)
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (\ctx -> FA (FG (happy_var_2 ctx))
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
		 (\ctx -> FX     (happy_var_2 ctx)
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  21 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (\ctx -> FF     (happy_var_2 ctx)
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  21 happyReduction_43
happyReduction_43 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (\ctx -> FG     (happy_var_2 ctx)
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 (\ctx -> FU     (happy_var_1 ctx) (happy_var_3 ctx)
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
happyReduction_74 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn28
		 (\ctx -> (RED happy_var_1 [], ctx)
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  28 happyReduction_75
happyReduction_75 (HappyAbsSyn31  happy_var_1)
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
happyReduction_81 (HappyTerminal (NUM happy_var_1))
	 =  HappyAbsSyn30
		 (let f n = case n of 
                                                        0 -> DZero 
                                                        _ -> DSucc (f (n-1)) in 
                                                    TmDATA (f happy_var_1)
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  31 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn31
		 (TmTRUE
	)

happyReduce_83 = happySpecReduce_1  31 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn31
		 (TmFALSE
	)

happyNewToken action sts stk [] =
	action 102 102 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LET' -> cont 32;
	TRUE -> cont 33;
	FALSE -> cont 34;
	NUM happy_dollar_dollar -> cont 35;
	EVENT -> cont 36;
	CONTRACT -> cont 37;
	METHOD -> cont 38;
	RETURN -> cont 39;
	BECOME -> cont 40;
	IF -> cont 41;
	THEN -> cont 42;
	ELSE -> cont 43;
	I8 -> cont 44;
	U8 -> cont 45;
	I256 -> cont 46;
	U256 -> cont 47;
	BOOL -> cont 48;
	CASE -> cont 49;
	NEW -> cont 50;
	CALL -> cont 51;
	SENDER -> cont 52;
	SEND -> cont 53;
	MSG -> cont 54;
	AMOUNT -> cont 55;
	BALANCE -> cont 56;
	DESTRUCT -> cont 57;
	THIS -> cont 58;
	NOW -> cont 59;
	UNIT -> cont 60;
	LOG -> cont 61;
	KECCAK -> cont 62;
	ARROW -> cont 63;
	DARROW -> cont 64;
	LPAREN -> cont 65;
	RPAREN -> cont 66;
	LBRACE -> cont 67;
	RBRACE -> cont 68;
	LSQUARE -> cont 69;
	RSQUARE -> cont 70;
	SEMI -> cont 71;
	COLON -> cont 72;
	EQ -> cont 73;
	COLONEQ -> cont 74;
	COMMA -> cont 75;
	DOT -> cont 76;
	NOT -> cont 77;
	GT -> cont 78;
	LT -> cont 79;
	GE -> cont 80;
	LE -> cont 81;
	EQEQ -> cont 82;
	NEQ -> cont 83;
	PLUS -> cont 84;
	MINUS -> cont 85;
	MULT -> cont 86;
	DIV -> cont 87;
	MOD -> cont 88;
	COMMENT happy_dollar_dollar -> cont 89;
	ID happy_dollar_dollar -> cont 90;
	POSSIBLY -> cont 91;
	NECESSARILY -> cont 92;
	NEXT -> cont 93;
	FUTURELY -> cont 94;
	GLOBALLY -> cont 95;
	UNTIL -> cont 96;
	ALWAYS -> cont 97;
	NEVER -> cont 98;
	BY -> cont 99;
	AND' -> cont 100;
	OR' -> cont 101;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 102 tk tks = happyError' (tks, explist)
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
