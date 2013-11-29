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

import Prelude hiding((++))
import qualified Prelude ((++))

import Control.Monad.Par
import qualified Prelude ((++))
import Data.Array.Repa (Z(..),DIM1,DIM2,Array,(!),(++),(+^),(-^),(*^),(/^))
import qualified Data.Array.Repa as A
import Data.Int

type Vector r e = Array r DIM1 e
type Matrix r e = Array r DIM2 e

type Attr  = Matrix A.U Double
-- Adjecency-Matrix
type Adj   = Matrix A.U Int8
type Constraints = (Vector A.U Int, Matrix A.U Double)
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

-- filterLayer removes all duplicate graphs
filterLayer :: Vector A.U Graph -> Vector A.U Graph
filterLayer gs = undefined

-- constraint gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
-- all constraints defined via the Attribute-Matrix.
constraint :: Adj -> Attr -> MaxDivergence -> Int -> Graph -> Int -> Maybe Constraints
constraint adj attr div req g newNode = undefined -- test each attribute -> sum -> test sum with req


-- addPoint gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
-- Adjacency-Matrix the Vector should represent to generate further Graphs
addPoint :: Adj -> Attr -> Density -> MaxDivergence -> Int -> Graph -> Int -> Maybe Graph
addPoint adj attr d div req g c = undefined -- call constraint, test (updated) density

-- addablePoints yields all valid addititons (= neighbours) to a Graph
addablePoints :: Adj -> Graph -> Vector A.U Int
addablePoints adj g = undefined
