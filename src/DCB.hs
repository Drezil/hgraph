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

--import Stream hiding (map) --same as Data.Stream imported above?
import Data.Array.Accelerate (Z(..), DIM1, DIM2, DIM3, Scalar, Vector, (:.)(..), Array,
    Int8, Int, Float, Double, Acc, Exp, Elt)
import qualified Data.Array.Accelerate as A
-- change to Data.Array.Accelerate.CUDA as I and link accelerate-cuda to use GPU instead of CPU
-- depends on accelerate-cuda package in cabal, which needs the installed CUDA-stuff form
-- nVidia (nvcc, header-files, ...) and the propriatary driver
import Data.Array.Accelerate.Interpreter as I
type Matrix e = Array DIM2 e

type Attr  = Matrix Double
-- Adjecency-Matrix
type Adj   = Matrix Int8
-- Vector of the Adjecency-Matrix
type AdjV  = Vector Int8
newtype Constraints = Matrix Double
-- Graph consists of a Vector denoting which colums of the matrix represents wich originating
-- column in the global adjencency-matrix, the reduces adjencency-matrix of the graph, a
-- matrix of constraints and a scalar denoting the density
type Density = Scalar Float

-- Graph
type Graph = (Vector Int8, Adj, Constraints, Density)

-- Vector of Graphs
type MultiGraph e = (Vector Int8, Array DIM3 e, Constraints, Density)



expand :: Acc (MultiGraph Int8)-> Acc Adj -> Acc Attr -> Acc (MultiGraph Int8)
expand g a att = undefined

-- constraint gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
-- all constraints defined via the Attribute-Matrix.
constraint :: Acc Graph -> Acc Attr -> Acc (Scalar Bool)
constraint g a = undefined


-- addPoint gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
-- Adjacency-Matrix the Vector should represent to generate further Graphs
addPoint :: Acc Graph -> Acc (Adj, (Scalar Int)) -> Acc (MultiGraph Int8)
addPoint g a = undefined


-- addablePoints yields all valid addititonsto a Graph
addablePoints :: Acc Adj -> Acc Graph-> Acc (Vector Int8)
addablePoints a g = undefined
