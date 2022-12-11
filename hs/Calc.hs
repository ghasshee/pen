-- Parser generated by Pappy - do not edit
module Calc where

import Data.Char
import Pos
import Parse

 

eval :: String -> Int 
eval str = case calcExpr (calcParse "expr" str) of 
    Parsed v _ _ -> v 
    NoParse e -> error (show e)



data CalcDerivs = CalcDerivs {
	calcSub0 :: CalcDerivs0,
	calcChar :: Result CalcDerivs Char,
	calcPos :: Pos
}

instance Derivs CalcDerivs where
	dvChar d = calcChar d
	dvPos d = calcPos d

data CalcDerivs0 = CalcDerivs0 {
	calcSubPlusTail :: Result CalcDerivs ((Int -> Int)),
	calcSubMultTail :: Result CalcDerivs ((Int -> Int)),
	calcSubDigitsTail :: Result CalcDerivs ((Int -> Int)),
	calcSubStarRule0 :: Result CalcDerivs ([Char])}

calcPlusTail = calcSubPlusTail . calcSub0
calcMultTail = calcSubMultTail . calcSub0
calcDigitsTail = calcSubDigitsTail . calcSub0
calcStarRule0 = calcSubStarRule0 . calcSub0

calcParse :: String -> String -> CalcDerivs
calcParse name text = calcDerivs (Pos name 1 1) text

calcDerivs :: Pos -> String -> CalcDerivs
calcDerivs pos text = dvs where
  dvs = CalcDerivs
    (calcDerivs0 dvs)
    chr pos
  chr = case text of
    [] -> NoParse (eofError dvs)
    (c:cs) -> Parsed c (calcDerivs (nextPos pos c) cs) (nullError dvs)

calcDerivs0 dvs = CalcDerivs0
	(calcParsePlusTail dvs)
	(calcParseMultTail dvs)
	(calcParseDigitsTail dvs)
	(calcParseStarRule0 dvs)

calcExpr :: CalcDerivs -> Result CalcDerivs (Int)
calcExpr d =
  case calcStarRule0 d of
    Parsed _ d1 e1 ->
      case pappyResult3 of
        Parsed v d3 e3 ->
          case calcChar d3 of
            NoParse e5 ->
              Parsed ( v ) d3 (maximum [e5,e3,e1])
            Parsed _ _ e5 -> NoParse (maximum [e5,e3,e1])
        NoParse e3 -> NoParse (max e3 e1)
      where
        pappyResult3 =
          case pappyResult5 of
            Parsed l d5 e5 ->
              case calcPlusTail d5 of
                Parsed t d7 e7 ->
                  Parsed (t l) d7 (max e7 e5)
                NoParse e7 -> NoParse (max e7 e5)
            NoParse e5 -> NoParse e5
          where
            pappyResult5 =
              case calcMult d1 of
                Parsed v d7 e7 ->
                  Parsed ( v          ) d7 e7
                NoParse e7 -> NoParse e7
    NoParse e1 -> NoParse e1

calcParsePlusTail :: CalcDerivs -> Result CalcDerivs ((Int -> Int))
calcParsePlusTail d =
  pappyAlt1_1 (ParseError (calcPos d) []) where
    pappyAlt1_1 e1 =
      case calcOp d of
        Parsed "+" d3 e3 ->
          case calcMult d3 of
            Parsed r d5 e5 ->
              case calcPlusTail d5 of
                Parsed pappyTail d7 e7 ->
                  Parsed (\l -> pappyTail ( l + r      )) d7 (maximum [e7,e5,e3,e1])
                NoParse e7 -> pappyAlt1_2 (maximum [e7,e5,e3,e1])
            NoParse e5 -> pappyAlt1_2 (maximum [e5,e3,e1])
        _ -> pappyAlt1_2 (max (ParseError (calcPos d) [Expected "+"]) e1)
    pappyAlt1_2 e1 =
      case calcOp d of
        Parsed "-" d3 e3 ->
          case calcMult d3 of
            Parsed r d5 e5 ->
              case calcPlusTail d5 of
                Parsed pappyTail d7 e7 ->
                  Parsed (\l -> pappyTail ( l - r      )) d7 (maximum [e7,e5,e3,e1])
                NoParse e7 -> pappyAlt1_3 (maximum [e7,e5,e3,e1])
            NoParse e5 -> pappyAlt1_3 (maximum [e5,e3,e1])
        _ -> pappyAlt1_3 (max (ParseError (calcPos d) [Expected "-"]) e1)
    pappyAlt1_3 e1 =
      Parsed (\v -> v) d e1
    pappyAlt1_4 e1 = NoParse e1

calcParseMultTail :: CalcDerivs -> Result CalcDerivs ((Int -> Int))
calcParseMultTail d =
  pappyAlt1_1 (ParseError (calcPos d) []) where
    pappyAlt1_1 e1 =
      case calcOp d of
        Parsed "*" d3 e3 ->
          case pappyResult5 of
            Parsed r d5 e5 ->
              case calcMultTail d5 of
                Parsed pappyTail d7 e7 ->
                  Parsed (\l -> pappyTail ( l * r      )) d7 (maximum [e7,e5,e3,e1])
                NoParse e7 -> pappyAlt1_2 (maximum [e7,e5,e3,e1])
            NoParse e5 -> pappyAlt1_2 (maximum [e5,e3,e1])
          where
            pappyResult5 =
              case calcDcml d3 of
                Parsed v d7 e7 ->
                  Parsed ( v          ) d7 e7
                NoParse e7 -> NoParse e7
        _ -> pappyAlt1_2 (max (ParseError (calcPos d) [Expected "*"]) e1)
    pappyAlt1_2 e1 =
      case calcOp d of
        Parsed "/" d3 e3 ->
          case pappyResult5 of
            Parsed r d5 e5 ->
              case calcMultTail d5 of
                Parsed pappyTail d7 e7 ->
                  Parsed (\l -> pappyTail ( l `div` r  )) d7 (maximum [e7,e5,e3,e1])
                NoParse e7 -> pappyAlt1_3 (maximum [e7,e5,e3,e1])
            NoParse e5 -> pappyAlt1_3 (maximum [e5,e3,e1])
          where
            pappyResult5 =
              case calcDcml d3 of
                Parsed v d7 e7 ->
                  Parsed ( v          ) d7 e7
                NoParse e7 -> NoParse e7
        _ -> pappyAlt1_3 (max (ParseError (calcPos d) [Expected "/"]) e1)
    pappyAlt1_3 e1 =
      Parsed (\v -> v) d e1
    pappyAlt1_4 e1 = NoParse e1

calcMult :: CalcDerivs -> Result CalcDerivs (Int)
calcMult d =
  case pappyResult1 of
    Parsed l d1 e1 ->
      case calcMultTail d1 of
        Parsed t d3 e3 ->
          Parsed (t l) d3 (max e3 e1)
        NoParse e3 -> NoParse (max e3 e1)
    NoParse e1 -> NoParse e1
  where
    pappyResult1 =
      case pappyResult3 of
        Parsed v d3 e3 ->
          Parsed ( v          ) d3 e3
        NoParse e3 -> NoParse e3
      where
        pappyResult3 =
          case calcDcml d of
            Parsed v d5 e5 ->
              Parsed ( v          ) d5 e5
            NoParse e5 -> NoParse e5

calcDcml :: CalcDerivs -> Result CalcDerivs (Int)
calcDcml d =
  case pappyResult1 of
    Parsed v d1 e1 ->
      case calcStarRule0 d1 of
        Parsed _ d3 e3 ->
          Parsed ( v          ) d3 (max e3 e1)
        NoParse e3 -> NoParse (max e3 e1)
    NoParse e1 -> NoParse e1
  where
    pappyResult1 =
      case pappyResult3 of
        Parsed l d3 e3 ->
          case calcDigitsTail d3 of
            Parsed t d5 e5 ->
              Parsed (t l) d5 (max e5 e3)
            NoParse e5 -> NoParse (max e5 e3)
        NoParse e3 -> NoParse e3
      where
        pappyResult3 =
          case calcDigit d of
            Parsed d d5 e5 ->
              Parsed ( d ) d5 e5
            NoParse e5 -> NoParse e5

calcParseDigitsTail :: CalcDerivs -> Result CalcDerivs ((Int -> Int))
calcParseDigitsTail d =
  pappyAlt1_1 (ParseError (calcPos d) []) where
    pappyAlt1_1 e1 =
      case calcDigit d of
        Parsed d d3 e3 ->
          case calcDigitsTail d3 of
            Parsed pappyTail d5 e5 ->
              Parsed (\v -> pappyTail ( v * 10 + d )) d5 (maximum [e5,e3,e1])
            NoParse e5 -> pappyAlt1_2 (maximum [e5,e3,e1])
        NoParse e3 -> pappyAlt1_2 (max e3 e1)
    pappyAlt1_2 e1 =
      Parsed (\v -> v) d e1
    pappyAlt1_3 e1 = NoParse e1

calcDigit :: CalcDerivs -> Result CalcDerivs (Int)
calcDigit d =
  case calcChar d of
    Parsed c d1 e1 ->
      case (isDigit c) of
        True ->
          Parsed ( digitToInt c ) d1 e1
        False -> NoParse e1
    NoParse e1 -> NoParse e1

calcOp :: CalcDerivs -> Result CalcDerivs (String)
calcOp d =
  case pappyResult1 of
    Parsed o d1 e1 ->
      case calcStarRule0 d1 of
        Parsed _ d3 e3 ->
          Parsed ( o          ) d3 (max e3 e1)
        NoParse e3 -> NoParse (max e3 e1)
    NoParse e1 -> NoParse e1
  where
    pappyResult1 =
      case calcChar d of
        Parsed '+' d3 _ ->
          Parsed ("+") d3 (ParseError (calcPos d3) [])
        Parsed '-' d3 _ ->
          Parsed ("-") d3 (ParseError (calcPos d3) [])
        Parsed '*' d3 _ ->
          Parsed ("*") d3 (ParseError (calcPos d3) [])
        Parsed '/' d3 _ ->
          Parsed ("/") d3 (ParseError (calcPos d3) [])
        _ -> NoParse (ParseError (calcPos d) [])

calcParseStarRule0 :: CalcDerivs -> Result CalcDerivs ([Char])
calcParseStarRule0 d =
  pappyAlt1_1 (ParseError (calcPos d) []) where
    pappyAlt1_1 e1 =
      case pappyResult3 of
        Parsed v d3 e3 ->
          case calcStarRule0 d3 of
            Parsed vs d5 e5 ->
              Parsed (v : vs) d5 (maximum [e5,e3,e1])
            NoParse e5 -> pappyAlt1_2 (maximum [e5,e3,e1])
        NoParse e3 -> pappyAlt1_2 (max e3 e1)
      where
        pappyResult3 =
          case calcChar d of
            Parsed c d5 e5 ->
              case (isSpace c) of
                True ->
                  Parsed ( c ) d5 e5
                False -> NoParse e5
            NoParse e5 -> NoParse e5
    pappyAlt1_2 e1 =
      Parsed ([]) d e1
    pappyAlt1_3 e1 = NoParse e1

