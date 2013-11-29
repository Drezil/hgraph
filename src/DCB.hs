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

import           Prelude hiding((++))
import qualified Prelude ((++))

import           Control.Monad.Par
import qualified Prelude ((++))
import           Data.Array.Repa (Array,(:.)(..),(!),(++),(+^),(-^),(*^),(/^))
import qualified Data.Array.Repa as A
import           Data.Array.Repa.Index
import           Data.Int
import           Data.Maybe

type Vector r e = Array r DIM1 e
type Matrix r e = Array r DIM2 e

type Attr  = Matrix A.U Double
-- Adjecency-Matrix
type Adj   = Matrix A.U Int8
type Constraints = (Vector A.U Int8, Matrix A.U Double)
-- Graph consists of a Vector denoting which colums of the matrix represents wich originating
-- column in the global adjencency-matrix, the reduces adjencency-matrix of the graph, a
-- matrix of constraints and a scalar denoting the density
type MaxDivergence = Vector A.U Double
type Density = Double

-- Graph
type Graph = (Vector A.U Int, Constraints, Density)

-- expand calculates all possible additions towards a vector of graphs
expand :: Adj -> Attr -> [Graph] ->  [Graph]
expand adj attr g = undefined -- addablePoints -> for each: addPoint -> filterLayer


preprocess :: Adj -> Attr -> Density -> MaxDivergence -> (Adj, Vector A.U Graph)
preprocess adj attr d div = undefined
--    let
--        (Z:.nNodes:._) = A.extract adj
--        pairs = A.fromFunction (ix1 (((nNodes-1)*(nNodes-2)) / 2)) (\(Z:.i) -> (i % nNodes))
--        finalGraphs = foo
--
--    in (adj, A.computeS finalGraphs)
-- TODO for all pairs (i, j) with adj(i,j) != 0: if initGraph add, else discard and update adjacancy matrix

-- initGraph initializes a seed graph if it fulfills the constraints
-- assumption: given nodes i, j are connected
initGraph :: Attr -> MaxDivergence -> Int -> Int -> Int -> Maybe Graph
initGraph attr div req i j =
    let
       constr = constraintInit attr div req i j
    in case constr of
            Nothing -> Nothing
            Just c  -> Just (A.computeS $A.fromFunction (ix1 2)
                    (\(Z:.i) -> if i == 0 then i else j), c, 1)

-- constraintInit checks the contraints for an initializin seed
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

-- filterLayer removes all duplicate graphs
filterLayer :: Vector A.U Graph -> Vector A.U Graph
filterLayer gs = undefined -- TODO

-- constraint gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
-- all constraints defined via the Attribute-Matrix.
constraint :: Attr -> MaxDivergence -> Int -> Graph -> Int -> Maybe Constraints
constraint attr div req (_, (fulfill, constr), _) newNode =
    let
        updateConstr :: (DIM2 -> Double) -> DIM2 -> Double
        updateConstr f sh@(Z:._:.c) =
            case c of
                 0 -> min (f sh) (attr!sh)
                 1 -> max (f sh) (attr!sh)
        constrNew = A.computeUnboxedS $A.traverse constr id updateConstr
        fulfillNew = A.zipWith (\i b -> if i == 1 && b then 1::Int8 else 0::Int8) fulfill
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

-- addPoint gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
-- Adjacency-Matrix the Vector should represent to generate further Graphs
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
                     True  -> Just (A.computeS $nodes ++ A.fromFunction (ix1 1) (\i -> n), c, densNew)
                     False -> Nothing

-- addablePoints yields all valid addititons (=neighbours) to a Graph
addablePoints :: Adj -> Graph -> Vector A.U Int
addablePoints adj g = undefined --TODO
