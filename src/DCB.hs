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

import Prelude hiding (Int, Double, Float)
import qualified Prelude (Int, Double, Float)

--import Stream hiding (map) --same as Data.Stream imported above?
import Data.Array.Accelerate (Z(..),DIM0,DIM1,DIM2,DIM3,Scalar,Vector,(:.)(..),Array,(!),(!!),
    Int8,Int,Float,Double,Acc,Exp,Elt,(>->),(>*),(<*),(>=*),(<=*),(==*),(/=*),(?),(?|),(&&*),(||*))
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
type AdjV  = Vector Int
newtype Constraints = Matrix Double
-- Graph consists of a Vector denoting which colums of the matrix represents wich originating
-- column in the global adjencency-matrix, the reduces adjencency-matrix of the graph, a
-- matrix of constraints and a scalar denoting the density
type Density = Scalar Double

-- Graph
type Graph = (Vector Int, Adj, Constraints, Density)

-- Vector of Graphs
type MultiGraph e = (Vector Int, Array DIM3 e, Constraints, Density)

-- Multigraph correct output ?
preprocess :: Acc (Matrix Int8) -> Acc Attr -> Acc (MultiGraph Int8)
preprocess adj a = undefined

-- tests whether the minimum amount of attributes are within range
-- first argument: required attributes to be in range
-- second argument: constraints vector with 1/0 entries for single attributes
testConstraints :: Acc (Scalar Int) -> Acc (Vector Int8) -> Exp Bool
testConstraints minHits = A.the . A.map (\s -> A.the minHits >=* A.fromIntegral s) . A.fold1All (+)

createConstrMat :: Acc Attr -> Acc (Vector Double) -> Acc (Vector Int)
        -> Acc ((Matrix Double), (Vector Int8))
createConstrMat attr maxDist nodes =
    let
        (Z:._:.nAttr) = A.unlift (A.shape attr) :: ((:.) ((:.) Z (Exp Int)) (Exp Int))
        constrMat = A.generate (A.index2 nAttr 3) (genConstrMat)
        -- generator function for the constraints fulfillment matrix
        -- first column contains minimum and second column maximum value of the attributes
        genConstrMat :: Exp DIM2 -> Exp Double
        genConstrMat ix =
            let
                (Z:.idAttr:.col) = A.unlift ix :: ((:.) ((:.) Z (Exp Int)) (Exp Int))
            in case col of
                    0 -> A.the $A.minimum (A.map (\i -> attr!(A.index2 i idAttr)) nodes)
                    1 -> A.the $A.maximum (A.map (\i -> attr!(A.index2 i idAttr)) nodes)
        -- tests whether an attribute is within the accepted threshold
        testDist :: Exp Int -> Exp Double -> Exp Int8
        testDist ix d = abs d <* maxDist!(A.index1 ix) ? (1, 0)
    in A.lift (constrMat, (A.fold1 ((-):: Exp Double -> Exp Double -> Exp Double)
            >-> A.zipWith testDist (A.enumFromN (A.index1 nAttr) 0)) constrMat)
            --subtract values >-> combine with vector of indices and test distance
            --TODO improvable by permute/backpermute?

--creates the new constraints fulfillment matrix when adding a new node to a known graph
updateConstrMatrix :: Acc Attr -> Acc (Vector Double) -> Acc (Scalar Int)
        -> Acc (Matrix Double, Vector Int8) -> Acc ((Matrix Double), (Vector Int8))
updateConstrMatrix attr maxDist node constr =
    let
        (Z:._:.nAttr) = A.unlift (A.shape attr) :: ((:.) ((:.) Z (Exp Int)) (Exp Int))
        (minmax, fulfill) = A.unlift constr :: (Acc (Matrix Double), Acc (Vector Int8))
        newConstr = A.generate (A.shape attr) genUpConstrMat
        genUpConstrMat :: Exp DIM2 -> Exp Double
        genUpConstrMat ix =
            let
                (Z:.idAttr:.col) = A.unlift ix :: ((:.) ((:.) Z (Exp Int)) (Exp Int))
            in case col of
                    0 -> A.min (attr!(A.index2 (A.the node) idAttr)) (minmax!(A.index2 idAttr 0))
                    1 -> A.max (attr!(A.index2 (A.the node) idAttr)) (minmax!(A.index2 idAttr 1))
        testUpDist :: Exp Int -> Exp Double -> Exp Int8
        testUpDist ix d =
            let
                dIx = A.index1 ix
            in fulfill!dIx ==* 1 &&* abs d <* maxDist!dIx ? (1, 0)
    in A.lift (newConstr, (A.fold1 ((-):: Exp Double -> Exp Double -> Exp Double)
            >-> A.zipWith testUpDist (A.enumFromN (A.index1 nAttr) 0)) newConstr)

expand :: Acc (MultiGraph Int8)-> Acc Adj -> Acc Attr -> Acc (MultiGraph Int8)
expand g a att = undefined

-- constraint gets a Graph and an Attribute-Matrix and yields true, if the Graph still fulfills
-- all constraints defined via the Attribute-Matrix.
--constraint :: Acc Graph -> Acc Attr -> Acc (Scalar Bool)
constraint :: Acc Graph -> Int -> Acc Attr -> Acc (Maybe Graph)
constraint g newNode a = undefined


-- addPoint gets a graph and a tuple of an adjecancy-Vector with an int wich column of the
-- Adjacency-Matrix the Vector should represent to generate further Graphs
addPoint :: Acc Graph -> Acc (Adj, (Scalar Int)) -> Acc (MultiGraph Int8)
addPoint g a = undefined


-- addablePoints yields all valid addititons to a Graph
addablePoints :: Acc Adj -> Acc Graph-> Acc (Vector Int8)
addablePoints a g = undefined
