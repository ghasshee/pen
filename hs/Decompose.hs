
{-# LANGUAGE ScopedTypeVariables #-}
module DirectSumDecompose where

import Data.Matrix (Matrix, nrows, ncols, (!), fromLists, toLists, submatrix, matrix)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (fromJust)

-- Build undirected adjacency from nonzero pattern:
-- nodes 1..n. Edge between i and j if A[i,j] /= 0 || A[j,i] /= 0 (i /= j).
adjacency :: (Eq a, Num a) => Matrix a -> IM.IntMap IS.IntSet
adjacency a
  | nrows a /= ncols a = error "adjacency: matrix must be square"
  | otherwise =
      let n = nrows a
          addEdge mp i j =
            if i == j then mp
            else mp
              & IM.alter (Just . maybe (IS.singleton j) (IS.insert j)) i
              & IM.alter (Just . maybe (IS.singleton i) (IS.insert i)) j
          -- fold over all pairs i,j and add edge if non-zero
          pairs = [(i,j) | i <- [1..n], j <- [1..n], i /= j]
      in foldl' (\mp (i,j) ->
                    if a ! (i,j) /= 0 || a ! (j,i) /= 0
                      then addEdge mp i j
                      else mp
                ) IM.empty pairs
  where
    (&) = flip ($)

-- Depth-first search to collect a component
dfs :: IM.IntMap IS.IntSet -> Int -> IS.IntSet -> IS.IntSet
dfs adj start visited =
  let go stack vis
        | null stack = vis
        | otherwise =
            let x:xs = stack
            in if IS.member x vis
                 then go xs vis
                 else
                   let neigh = IM.findWithDefault IS.empty x adj
                       newStack = IS.toList neigh ++ xs
                       newVis = IS.insert x vis
                   in go newStack newVis
  in go [start] visited

-- Compute connected components (as lists in increasing order)
connectedComponents :: IM.IntMap IS.IntSet -> [[Int]]
connectedComponents adj =
  let nodes = IS.toList $ IS.unions $ IM.elems adj `orEmpty` IM.keysSet adj
      -- helper to gather all nodes: include isolated nodes (no edges)
      orEmpty :: [IS.IntSet] -> IS.IntSet -> IS.IntSet
      orEmpty sets keys = foldl' IS.union keys sets
      -- but above is awkward; simpler: collect all possible indices from keys and adjacency sets
      allNodes :: IS.IntSet
      allNodes = IS.unions (IM.keysSet adj : IM.elems adj)
      starts = IS.toList allNodes
      go [] _ acc = reverse acc
      go (s:ss) visited acc
        | IS.member s visited = go ss visited acc
        | otherwise =
            let compSet = dfs adj s visited
                visited' = IS.union visited compSet
                compList = IS.toList compSet
            in go ss visited' (compList:acc)
  in go starts IS.empty []

-- Extract blocks according to components (each component is list of 1-based indices)
extractBlocks :: forall a. Matrix a -> [[Int]] -> [Matrix a]
extractBlocks m comps =
  map (\idxs ->
        let k = length idxs
            -- build k x k matrix where (r,c) maps to original (idxs!!(r-1), idxs!!(c-1))
            mk = matrix k $ \(r,c) ->
                   m ! ( idxs !! (r-1), idxs !! (c-1) )
        in mk
      ) comps

-- Top-level: detect components and extract blocks.
-- Returns (components_in_order, blocks)
directSumDecompose :: (Eq a, Num a) => Matrix a -> ([[Int]], [Matrix a])
directSumDecompose m
  | nrows m /= ncols m = error "directSumDecompose: require square matrix"
  | otherwise =
      let n = nrows m
          -- adjacency map (if a node is isolated (no edges), ensure it appears alone)
          adj0 = adjacency m
          allNodesSet = IS.fromList [1..n]
          adj = foldl' (\mp i -> IM.insertWith (\_ old -> old) i IS.empty mp) adj0 [1..n]
          comps = connectedComponents adj
          -- sort indices inside each component for deterministic output
          compsSorted = map (Prelude.foldr insertSorted [] ) comps
          blocks = extractBlocks m compsSorted
      in (compsSorted, blocks)
  where
    insertSorted x [] = [x]
    insertSorted x ys@(y:ys')
      | x < y     = x:ys
      | otherwise = y : insertSorted x ys'

-- ----------------------------
-- Example usage
-- ----------------------------
example :: IO ()
example = do
  -- A 6x6 matrix that is block diagonal up to permutation:
  -- Blocks: rows/cols {1,3}, {2,4,5}, {6}
  let a = fromLists
            [ [1,0,2,0,0,0]
            , [0,3,0,4,5,0]
            , [6,0,7,0,0,0]
            , [0,8,0,9,10,0]
            , [0,11,0,12,13,0]
            , [0,0,0,0,0,14]
            ]
  let (comps, blocks) = directSumDecompose a
  putStrLn $ "components: " ++ show comps
  putStrLn $ "number of blocks: " ++ show (length blocks)
  mapM_ (\(i,b) -> do
            putStrLn $ "block " ++ show i ++ ":"
            mapM_ print (toLists b)
        ) (zip [1..] blocks)
