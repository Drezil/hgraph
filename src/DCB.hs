{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  DCB
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module DCB where

import           Prelude               hiding ((++))
import qualified Prelude               ((++))

import           Control.Monad.Par
import           Data.Array.Repa       ((:.) (..), Array, (!), (*^), (++), (+^),
                                        (-^), (/^))
import qualified Data.Array.Repa       as A
import           Data.Array.Repa.Index
import           Data.Either
import           Data.Int
import qualified Data.Vector.Unboxed   as V
import           Debug.Trace

-- | a one-dimensional array
type Vector r e = Array r DIM1 e
-- | a two-dimensional array
type Matrix r e = Array r DIM2 e

-- | A 'Matrix' of attribute values assigned to a graph’s nodes.
--   Each row contains the corresponding node’s attribute values.
type Attr  = Matrix A.U Double
-- | Adjacency-Matrix
type Adj   = Matrix A.U Int16

-- | Matrix storing the extent of a 'Graph'’s constraints fulfillment.
--   It stores the minimum (zeroth column) and maximum (first column) value of all
--   the 'Graph'’s nodes per attribute.
--   The 'Vector' stores values of @1@ if the bounds are within the allowed range
--   ragarding the corresponding attribute, or @0@ if not.
type Constraints = (Vector A.U Int16, Matrix A.U Double)
-- | A 'Vector' of weights indicating how much divergence is allowed in which dimension.
--   Each dimension represents an attribute.
type MaxDivergence = Vector A.U Double
-- | A graph’s density.
type Density = Double

-- | consists of a 'Vector' denoting which columns of the 'Matrix' represents which originating
--   column in the global adjancency-matrix, a 'Matrix' of 'Constraints' and a scalar denoting the graph’s 'Density'
type Graph = (Vector A.U Int, Constraints, Density)

instance Ord Graph where
        (nodes, _, _) `compare` (nodes', _, _) = (A.size $ A.extent nodes) `compare` (A.size $ A.extent nodes')



testAdj :: Adj
testAdj = A.fromListUnboxed (ix2 10 10) [0,1,1,1,0,0,1,0,0,1,{----}1,0,0,0,1,0,1,1,0,0,
                                         1,0,0,1,0,0,0,1,0,1,{----}1,0,1,0,1,1,1,0,0,0,
                                         0,1,0,1,0,0,1,1,0,0,{----}0,0,0,1,0,0,1,0,1,1,
                                         1,1,0,1,1,1,0,0,1,0,{----}0,1,1,0,1,0,0,0,0,1,
                                         0,0,0,0,0,1,1,0,0,1,{----}1,0,1,0,0,1,0,1,1,0]
testAttr :: Attr
testAttr = A.fromListUnboxed (ix2 10 5) [   0.2,  1.3,   -1.4,   0.3, 0.0,
                                           -0.3, 33.0,   0.0,  -2.3, 0.1,
                                           -1.1,-12.0,   2.3,   1.1, 3.2,
                                            0.1,  1.7,   3.1,   0.7, 2.5,
                                            1.4, 35.1,  -1.1,   1.6, 1.4,
                                            0.5, 13.4,  -0.4,   0.5, 2.3,
                                            0.9, 13.6,   1.1,   0.1, 1.9,
                                            1.2, 12.9,  -0.5,  -0.3, 3.3,
                                            3.1,  2.4,  -0.1,   0.7, 0.4,
                                            2.6, -7.4,  -0.4,   1.3, 1.2]
testDivergence :: MaxDivergence
testDivergence = A.fromListUnboxed (ix1 5) [3.0, 0.0, 300.0, 2.0, 10.0]

testDensity = 0.7::Density;
testReq = 3 ::Int


-- | calculates all possible additions to one Graph, yielding a list of valid expansions
--   i.e. constraint a == Just Constraints for all returned Graphs
expand :: Adj -> Attr -> Graph ->  [Graph]
expand adj attr g = undefined -- addablePoints -> for each: addPoint -> filterLayer

-- | Creates an adjacency matrix from the given adjacency matrix where all
--   edges are removed whose belonging nodes cannot fulfill the passed constraints.
--   Additionally, all pairs of connected nodes that satisfy the constraints are
--   returned as a 'Graph'.
preprocess :: Adj           -- ^ original adjacency matrix
           -> Attr          -- ^ table of the node’s attributes
	       -> MaxDivergence -- ^  maximum allowed ranges of the node’s attribute
	                        --   values to be considered as consistent
	       -> Int           -- ^ required number of consistent attributes
	       -> (Adj, [Graph])
preprocess adj attr div req =
    let
        (Z:.nNodes:._) = A.extent adj
        results = map (initGraph attr div req) [(i, j) | i <- [0..(nNodes-1)], j <- [(i+1)..(nNodes-1)], adj!(ix2 i j) /= 0]
        finalGraphs = lefts results
        mask = A.fromUnboxed (A.extent adj) $V.replicate (nNodes*nNodes) False V.//
                ((map (\(i,j) -> (i*nNodes + (mod j nNodes), True)) $rights results)
                Prelude.++ (map (\(i,j) -> (j*nNodes + (mod i nNodes), True)) $rights results))
        adj' = A.computeS $A.fromFunction (A.extent adj) (\sh -> if mask!sh then 0 else adj!sh)
    in (adj', finalGraphs)

-- | Initializes a seed 'Graph' if it fulfills the constraints, returns the input nodes
--   otherwise. It is assumed that the given nodes are connected.
initGraph :: Attr                    -- ^ table of all node’s attributes
          -> MaxDivergence
          -> Int                     -- ^ required number of consistent attributes
          -> (Int, Int)              -- ^ nodes to create a seed 'Graph' of
          -> Either Graph (Int, Int)
initGraph attr div req (i, j) =
    let
       constr = constraintInit attr div req i j
    in case constr of
            Nothing -> Right (i, j)
            Just c  -> Left (A.fromListUnboxed (ix1 2) [i,j], c, 1)

-- | checks constraints of an initializing seed and creates 'Constraints' matrix if the
--   check is positive
constraintInit :: Attr -> MaxDivergence -> Int -- ^ required number of consistent attributes
               -> Int -- ^ first node to test
               -> Int -- ^ second node to test first node against
               -> Maybe Constraints
constraintInit attr div req i j =
    let
        (Z:._:.nAttr) = A.extent attr
        fConstr (Z:.a:.c) =
            let
                col = A.slice attr (A.Any:.a)
            in case c of
                    0 -> min (attr!(ix2 i a)) (attr!(ix2 j a))
                    1 -> max (attr!(ix2 i a)) (attr!(ix2 j a))
        constr = A.computeS $A.fromFunction (ix2 nAttr 2) fConstr
        fulfill = A.zipWith (\thediv dist -> if abs dist <= thediv then 1 else 0) div
                $A.foldS (-) 0 constr
        nrHit = A.foldAllS (+) (0::Int) $A.map fromIntegral fulfill
    in if nrHit >= req then Just (A.computeS fulfill, constr) else Nothing

-- | removes all duplicate graphs
filterLayer :: Vector A.U Graph -> Vector A.U Graph
filterLayer gs = undefined -- TODO

-- | Checks whether a given base 'Graph' can be extended by a single node and
--   the resulting 'Graph' still satisfies the given attribute constraints.
--   In case of a successful expansion the updated 'Constraints' matrix is returned.
constraint :: Attr -> MaxDivergence -> Int -- ^ required number of consistent attributes
           -> Graph -- ^ base graph
           -> Int   -- ^ node to extend base graph by
           -> Maybe Constraints
constraint attr div req (_, (fulfill, constr), _) newNode =
    let
        updateConstr :: (DIM2 -> Double) -> DIM2 -> Double
        updateConstr f sh@(Z:._:.c) =
            case c of
                 0 -> min (f sh) (attr!sh)
                 1 -> max (f sh) (attr!sh)
        constrNew = A.computeUnboxedS $A.traverse constr id updateConstr
        fulfillNew = A.zipWith (\i b -> if i == 1 && b then 1::Int16 else 0::Int16) fulfill
                $A.zipWith (\thediv dist -> abs dist <= thediv) div $A.foldS (-) 0 constrNew
        nrHit = A.foldAllS (+) (0::Int) $A.map fromIntegral fulfillNew
    in if nrHit >= req then Just (A.computeS fulfillNew, constrNew) else Nothing

-- updates the density of a graph extended by a single node
updateDensity :: Adj            -- ^ global adjacency matrix of all nodes
              -> Vector A.U Int -- ^ nodes of the base graph
              -> Int            -- ^ node to extend the graph by
              -> Density        -- ^ current density of base graph
              -> Density        -- ^ new density of expanded graph
updateDensity adj nodes newNode dens =
    let
        neighbours = A.foldAllS (+) (0::Int)
                $A.traverse nodes id (\f sh -> fromIntegral $adj!(ix2 (f sh) newNode))
        (Z:.n') = A.extent nodes
        n = fromIntegral n'
    in (dens * (n*(n+1)) / 2 + fromIntegral neighbours) * 2 / ((n+1)*(n+2))


-- | Checks a 'Graph' expansion with a single node regarding both the attribute constraints
--   and a minimum density. If it passes the test the extended graph is returned.
addPoint :: Adj           -- ^ global adjacency matrix of all nodes
         -> Attr          -- ^ global attribute matrix
         -> Density       -- ^ required minimum graph’s density
         -> MaxDivergence -- ^ allowed divergence per attribute
         -> Int           -- ^ equired number of consistent attributes
         -> Graph         -- ^ base graph
         -> Int           -- ^ node to extend base graph by
         -> Maybe Graph
addPoint adj attr d div req g@(nodes, _, dens) n =
    let
        constr = constraint attr div req g n
        densNew = updateDensity adj nodes n dens
    in
        case constr of
             Nothing  -> Nothing
             (Just c) ->
                case dens >= d of
                     True  -> Just (A.computeS $nodes ++ A.fromListUnboxed (ix1 1) [n], c, densNew)
                     False -> Nothing

-- | yields all valid addititons (=neighbours) to a Graph
addablePoints :: Adj -> Graph -> Vector A.U Int
addablePoints adj g = undefined --TODO
