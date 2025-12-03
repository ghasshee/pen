{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
-- ltl_to_buchi.hs
-- Simple implementation of LTL -> Generalized Buchi (GNBA) -> Buchi (NBA)
-- based on the tableau / Gerth et al. construction.
-- Note: educational, not optimized for huge formulas.

module LTLToBuchi where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Control.Monad (guard)

-- ===== LTL AST =====

data LTL
  = Top
  | Bot
  | AP String
  | Not LTL
  | And LTL LTL
  | Or  LTL LTL
  | X   LTL        -- next
  | U   LTL LTL    -- until
  | R   LTL LTL    -- release
  deriving (Eq, Ord, Show, Generic)

-- Convert to negation normal form (NNF): push Not down to atoms
nnf :: LTL -> LTL
nnf Top = Top
nnf Bot = Bot
nnf (AP s) = AP s
nnf (Not Top) = Bot
nnf (Not Bot) = Top
nnf (Not (AP s)) = Not (AP s)
nnf (Not (Not phi)) = nnf phi
nnf (Not (And a b)) = Or (nnf (Not a)) (nnf (Not b))
nnf (Not (Or a b)) = And (nnf (Not a)) (nnf (Not b))
nnf (Not (X a)) = X (nnf (Not a))  -- X is temporal but we keep negation but we will assume input uses only NNF for temporal: better translate Not X p to X Not p
nnf (Not (U a b)) = R (nnf (Not a)) (nnf (Not b))
nnf (Not (R a b)) = U (nnf (Not a)) (nnf (Not b))
nnf (And a b) = And (nnf a) (nnf b)
nnf (Or a b) = Or (nnf a) (nnf b)
nnf (X a) = X (nnf a)
nnf (U a b) = U (nnf a) (nnf b)
nnf (R a b) = R (nnf a) (nnf b)

-- NOTE: above pattern-splitting helpers were accidental; fix by full definition:

nnf' :: LTL -> LTL
nnf' Top = Top
nnf' Bot = Bot
nnf' (AP s) = AP s
nnf' (Not Top) = Bot
nnf' (Not Bot) = Top
nnf' (Not (AP s)) = Not (AP s)
nnf' (Not (Not phi)) = nnf' phi
nnf' (Not (And a b)) = Or (nnf' (Not a)) (nnf' (Not b))
nnf' (Not (Or a b)) = And (nnf' (Not a)) (nnf' (Not b))
nnf' (Not (X a)) = X (nnf' (Not a))
nnf' (Not (U a b)) = R (nnf' (Not a)) (nnf' (Not b))
nnf' (Not (R a b)) = U (nnf' (Not a)) (nnf' (Not b))
nnf' (And a b) = And (nnf' a) (nnf' b)
nnf' (Or a b) = Or (nnf' a) (nnf' b)
nnf' (X a) = X (nnf' a)
nnf' (U a b) = U (nnf' a) (nnf' b)
nnf' (R a b) = R (nnf' a) (nnf' b)


-- ===== closure =====

-- Collect subformulas and their negations (in NNF we keep negation only on AP)
closure :: LTL -> Set LTL
closure phi = go phi Set.empty
  where
    go f acc
      | f `Set.member` acc = acc
      | otherwise =
          let acc' = Set.insert f acc in
          case f of
            Top         -> acc'
            Bot         -> acc'
            AP _        -> acc'
            Not (AP _)  -> acc'
            Not _       -> error "closure: formula not in NNF"
            And a b     -> go a (go b acc')
            Or  a b     -> go a (go b acc')
            X a         -> go a acc'
            U a b       -> go a (go b acc')
            R a b       -> go a (go b acc')

-- Get all 'Until' subformulas (for acceptance sets)
untils :: Set LTL -> [LTL]
untils cl = [ u | u@(U _ _) <- Set.toList cl ]



-- ===== State (maximal consistent sets) generation =====

-- We'll make states as subsets of closure that are locally consistent wrt booleans.

isLiteral :: LTL -> Bool
isLiteral (AP _)        = True
isLiteral (Not (AP _))  = True
isLiteral Top           = True
isLiteral Bot           = True
isLiteral _             = False

-- Check local consistency for a candidate set A
consistentBool :: Set LTL -> Set LTL -> Bool
consistentBool cl a =
  -- no contradiction: not (p and not p)
  all noContradiction (Set.toList a)
  where
    noContradiction f = case f of
      Not (AP s) -> not (AP s `Set.member` a)
      AP s       -> not (Not (AP s) `Set.member` a)
      And p q    -> (And p q `Set.member` a) == (p `Set.member` a && q `Set.member` a)
      Or p q     -> (Or p q `Set.member` a) == (p `Set.member` a || q `Set.member` a)
      _          -> True

-- Generate candidate states: all subsets of closure that satisfy boolean constraints and are maximal
-- For simplicity, we generate all subsets and filter; note: exponential.

allStates :: Set LTL -> [Set LTL]
allStates cl =
  let cls = Set.toList cl
      power [] = [Set.empty]
      power (x:xs) = let rest = power xs in rest ++ map (Set.insert x) rest
      candidates = power cls
  in filter (\a -> consistentBool cl a && respectsAndOr cl a) candidates

-- ensure And/Or expansion consistency for all And/Or in closure
respectsAndOr :: Set LTL -> Set LTL -> Bool
respectsAndOr cl a = all check (Set.toList cl)
  where
    check (And p q) = (And p q `Set.member` a) == (p `Set.member` a && q `Set.member` a)
    check (Or p q)  = (Or p q `Set.member` a) == (p `Set.member` a || q `Set.member` a)
    check _ = True

-- Initial states: those containing the original formula
initialStates :: LTL -> [Set LTL]
initialStates phi =
  let phiNNF = nnf' phi
      cl = closure phiNNF
  in filter (\s -> phiNNF `Set.member` s) (allStates cl)

-- ===== Transitions =====

-- A -> B iff for all X p in A, p in B, and for all propositions, valuations align (we treat valuation lazily)

stepOK :: Set LTL -> Set LTL -> Set LTL -> Bool
stepOK cl a b = 
  -- next constraints
  
  all (\f  -> case f of
                X p -> p `Set.member` b
                _   -> True) (Set.toList a)

  -- propositional compatibility: if p in a and p is atomic, nothing else
  && noPropConflict a b
--}
noPropConflict :: Set LTL -> Set LTL -> Bool
noPropConflict a b =
  -- If AP p in a, nothing forces anything in b except nexts; we keep no extra constraint here
  True

transitions :: Set LTL -> [Set LTL] -> [ (Set LTL, Set LTL) ]
transitions cl states = do
  a <- states
  b <- states
  guard (stepOK cl a b)
  return (a,b)

-- ===== Acceptance: GNBA =====

type GNBA state =   ( [state]  -- states
                    , [state]  -- initial states (subset)
                    , [(state, state)] -- transitions
                    , [Set state] -- acceptance sets (each is set of states)
                    )

buildGNBA :: LTL -> GNBA (Set LTL)
buildGNBA phi =
  let phiNNF = nnf' phi
      cl = closure phiNNF
      sts = allStates cl
      inits = filter (\s -> phiNNF `Set.member` s) sts
      trans = transitions cl sts
      us = untils cl
      acc = map (acceptSetForUntil sts) us
  in (sts, inits, trans, acc)

-- For an until u = (phi U psi), acceptance set is states where psi holds
acceptSetForUntil :: [Set LTL] -> LTL -> Set (Set LTL)
acceptSetForUntil sts (U _ psi) = Set.fromList [ s | s <- sts, psi `Set.member` s ]
acceptSetForUntil _ _ = Set.empty

-- ===== GNBA -> NBA conversion (standard product over acceptance indices) =====

type NBA state = ( [ (state, Int) ]  -- states
                 , [ (state, Int) ]  -- initials
                 , [ ((state, Int), (state, Int)) ] -- transitions
                 , Set ( (state, Int) ) -- acceptance set
                 )

gnbaToNba :: (Ord state) => GNBA state -> NBA state
gnbaToNba (sts, inits, trans, accSets) =
  let k = length accSets
      statesN = [ (s,i) | s <- sts, i <- [0..k-1] ]
      initsN = [ (s,0) | s <- inits ]
      -- transition: (s,i) -> (t, j) where (s,t) in trans and j = if t in acc_i then (i+1) mod k else i
      transN = do
        ((s,t)) <- trans
        i <- [0..k-1]
        let j = if t `Set.member` (accSets !! i) then (i+1) `mod` k else i
        return ((s,i),(t,j))
      accN = Set.fromList [ (s,i) | s <- sts, i <- [0..k-1], s `Set.member` (accSets !! i) ]
  in (statesN, initsN, transN, accN)

-- ===== Pretty printing =====

showLTL :: LTL -> String
showLTL Top             = "⊤"
showLTL Bot             = "⊥"
showLTL (AP s)          = s
showLTL (Not (AP s))    = '!' : s
showLTL (Not _)         = "(neg?)"
showLTL (And a b)       = "(" ++ showLTL a ++ " & " ++ showLTL b ++ ")"
showLTL (Or a b)        = "(" ++ showLTL a ++ " | " ++ showLTL b ++ ")"
showLTL (X a)           = "X " ++ showLTL a
showLTL (U a b)         = "(" ++ showLTL a ++ " U " ++ showLTL b ++ ")"
showLTL (R a b)         = "(" ++ showLTL a ++ " R " ++ showLTL b ++ ")"

showState :: Set LTL -> String
showState s = "{" ++ intercalate "," (map showLTL (Set.toList s)) ++ "}"

printGNBA :: GNBA (Set LTL) -> IO ()
printGNBA (sts, inits, trans, accs) = do
  putStrLn "States:"
  mapM_ (putStrLn . showState) sts
  putStrLn "Initials:"
  mapM_ (putStrLn . showState) inits
  putStrLn "Transitions:"
  mapM_ (\(a,b) -> putStrLn $ showState a ++ " -> " ++ showState b) trans
  putStrLn "Acceptance sets:"
  mapM_ (\s -> putStrLn $ "-- set: " ++ intercalate "," (map showState (Set.toList s))) accs

printNBA :: (Show state) => NBA state -> IO ()
printNBA (sts, inits, trans, acc) = do
  putStrLn "NBA States:"
  mapM_ (putStrLn . show) sts
  putStrLn "Initials:"
  mapM_ (putStrLn . show) inits
  putStrLn "Transitions:"
  mapM_ (putStrLn . show) trans
  putStrLn "Acceptance states:"
  mapM_ (putStrLn . show) (Set.toList acc)

-- ===== Example =====

-- Example: G(p -> F q) which is equivalent to !( true U (p & ! (true U q)) ) but we'll make a small example
-- Simpler: phi = (p U q)

p :: LTL
p = AP "p"
q :: LTL
q = AP "q"

example     :: LTL
example     = U p q

runExample  :: IO ()
runExample  = do
  let phi   = example
      gnba  = buildGNBA phi  
  putStrLn  "=== GNBA ==="
  printGNBA gnba 
  let nba   = gnbaToNba gnba 
  putStrLn  "=== NBA ==="
  printNBA  nba

