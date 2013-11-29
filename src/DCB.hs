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
type Density = Double

-- Graph
type Graph = (Vector A.U Int, Constraints, Density)


expand :: Adj -> Attr -> [Graph] ->  [Graph]
expand adj attr g = undefined

-- constraint gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
-- all constraints defined via the Attribute-Matrix.
constraint :: Adj -> Attr -> Graph -> Int -> Maybe Bool
constraint adj attr g newNode = undefined


-- addPoint gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
-- Adjacency-Matrix the Vector should represent to generate further Graphs
addPoint :: Adj -> Attr -> Density -> Graph -> Int -> Maybe Graph
addPoint adj attr g c = undefined

-- addablePoints yields all valid addititons to a Graph
addablePoints :: Adj -> Graph -> Vector A.U Int
addablePoints adj g = undefined
