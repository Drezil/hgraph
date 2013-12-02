{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances, TypeOperators #-}
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

type Vector r e = Array r DIM1 e
type Matrix r e = Array r DIM2 e

type Attr  = Matrix A.U Double
-- | Adjacency-Matrix
type Adj   = Matrix A.U Int8

-- | Matrix of constraints

--TODO: Haddoc!
type Constraints = (Vector A.U Int, Matrix A.U Double)
-- | A vector of weights indicating how much divergence is allowed in which dimension
type MaxDivergence = Vector A.U Double
-- | Make this special Scalar explicitly visible
type Density = Double

-- | consists of a Vector denoting which columns of the matrix represents which originating
--   column in the global adjancency-matrix, a matrix of constraints and a scalar denoting the density
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

--TODO: Haddoc!
--Was macht der Int?
preprocess :: Adj -> Attr -> Density -> MaxDivergence -> Int -> (Adj, [Graph])
preprocess adj attr d div req =
    let
        (Z:.nNodes:._) = A.extent adj
        results = map (initGraph attr div req) [(i, j) | i <- [0..(nNodes-1)], j <- [(i+1)..(nNodes-1)], adj!(ix2 i j) /= 0]
        finalGraphs = lefts results
        mask = A.fromUnboxed (A.extent adj) $V.replicate (nNodes*nNodes) False V.//
                ((map (\(i,j) -> (i*nNodes + (mod j nNodes), True)) $rights results)
                Prelude.++ (map (\(i,j) -> (j*nNodes + (mod i nNodes), True)) $rights results))
        adj' = A.computeS $A.fromFunction (A.extent adj) (\sh -> if mask!sh then 0 else adj!sh)
    in (adj', finalGraphs)

-- | initializes a seed graph if it fulfills the constraints
--   assumption: given nodes i, j are connected
initGraph :: Attr -> MaxDivergence -> Int -> (Int, Int) -> Either Graph (Int, Int)
initGraph attr div req (i, j) =
    let
       constr = constraintInit attr div req i j
    in case constr of
            Nothing -> Right (i, j)
            Just c  -> Left (A.fromListUnboxed (ix1 2) [i,j], c, 1)

-- | checks constraints of an initializing seed
constraintInit :: Attr -> MaxDivergence -> Int -> Int -> Int -> Maybe Constraints
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

-- | gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
--   all constraints defined via the Attribute-Matrix.
constraint :: Attr -> MaxDivergence -> Int -> Graph -> Int -> Maybe Constraints
constraint attr div req (_, (fulfill, constr), _) newNode =
    let
        updateConstr :: (DIM2 -> Double) -> DIM2 -> Double
        updateConstr f sh@(Z:._:.c) =
            case c of
                 0 -> min (f sh) (attr!sh)
                 1 -> max (f sh) (attr!sh)
        constrNew = A.computeUnboxedS $A.traverse constr id updateConstr
        fulfillNew = A.zipWith (\i b -> if i == 1 && b then 1::Int else 0::Int) fulfill
                $A.zipWith (\thediv dist -> abs dist <= thediv) div $A.foldS (-) 0 constrNew
        nrHit = A.foldAllS (+) (0::Int) $A.map fromIntegral fulfillNew
    in if nrHit >= req then Just (A.computeS fulfillNew, constrNew) else Nothing


updateDensity :: Adj -> Vector A.U Int -> Int -> Density -> Density
updateDensity adj nodes newNode dens =
    let
        neighbours = A.foldAllS (+) (0::Int)
                $A.traverse nodes id (\f sh -> fromIntegral $adj!(ix2 (f sh) newNode))
        (Z:.n') = A.extent nodes
        n = fromIntegral n'
    in (dens * (n*(n+1)) / 2 + fromIntegral neighbours) * 2 / ((n+1)*(n+2))

-- | gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
--   Adjacency-Matrix the Vector should represent to generate further Graphs
addPoint :: Adj -> Attr -> Density -> MaxDivergence -> Int -> Graph -> Int -> Maybe Graph
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
addablePoints :: Adj -> Graph -> Vector A.U Int8
addablePoints adj (ind,_,_) = A.computeS $ 
                                        A.traverse 
                                                adj 
                                                reduceDim 
                                                (foldOr ind)
           where

                reduceDim :: (A.Shape sh, Integral a) => (sh :. a) -> sh
                reduceDim (a :. b) = a --A.shapeOfList $ tail $ A.listOfShape a

                foldOr :: (A.Shape sh', Num a) => Vector A.U Int -> ((sh' :. Int) -> a) -> sh' -> a
                foldOr indlist lookup ind = foldl1 (+) [lookup (ind :. i) | i <- (map fromIntegral (A.toList indlist))]


 