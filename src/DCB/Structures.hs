{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DCB.Structures where

import           Data.Array.Repa as A hiding ((++))
import           Data.Int
import           Util


-- | a one-dimensional array
type Vector r e = Array r DIM1 e
-- | a two-dimensional array
type Matrix r e = Array r DIM2 e

-- | A 'Matrix' of attribute values assigned to a graph’s nodes.
--   Each row contains the corresponding node’s attribute values.
type Attr  = Matrix A.U Double
-- | Adjacency-Matrix
type Adj   = Matrix A.U Int8

-- | Matrix storing the extent of a 'Graph'’s constraints fulfillment.
--   It stores the minimum (zeroth column) and maximum (first column) value of all
--   the 'Graph'’s nodes per attribute.
--   The 'Vector' stores values of @1@ if the bounds are within the allowed range
--   ragarding the corresponding attribute, or @0@ if not.
type Constraints = (Vector A.U Int, Matrix A.U Double)
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


