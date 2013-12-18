{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
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
--DCB.DCB---------------------------------------------------------------------------

module DCB.DCB where
import           Util
import           DCB.Structures
import           DCB.IO

import           Prelude                        hiding ((++))
import qualified Prelude                        as P ((++))

import           Control.Monad.Par
import           Control.Parallel.Strategies
import           Control.Monad.Identity
import           Control.DeepSeq
import           Data.Array.Repa                ((:.) (..), Array, (!), (*^), (++), (+^),
                                                (-^), (/^))
import qualified Data.Array.Repa                as A
import           Data.Array.Repa.Index
import           Data.Either
import           Data.Int
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Vector.Unboxed            as V
--import           Debug.Trace
import qualified Data.ByteString.Char8          as B




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

force :: (A.Shape sh, V.Unbox e) => Array A.D sh e -> Array A.U sh e
force a = runIdentity (A.computeP a) 

--ignore A.U-Array in deepseq - already unboxed..
instance (A.Shape sh, V.Unbox e) => NFData (Array A.U sh e) where
  rnf a = ()
  {-# INLINE rnf #-}

--TODO: Do we have to filter?

step :: [Graph] -> Adj -> Attr -> Density -> MaxDivergence -> Int -> [Graph]
step gs@((ind,_,_):_) a b c d e = --trace ("step from " P.++ show (A.extent ind) ) $ 
                                  filterLayer $ concat $ map (expand a b c d e ) gs
                                                        +|| (parBuffer 75 rdeepseq)
                                                                 


-- | calculates all possible additions to one Graph, yielding a list of valid expansions
--   i.e. constraint a == Just Constraints for all returned Graphs
expand :: Adj -> Attr -> Density -> MaxDivergence -> Int -> Graph ->  [Graph]
expand adj attr d div req g@(ind,_,_) = --trace ("expanding graph "P.++ B.unpack (outputGraph [g])) 
                                        catMaybes $ map
                                                        (addPoint adj attr d div req g)
                                                        (V.toList $ V.findIndices (==True) $ A.toUnboxed $ addablePoints adj g)

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
        ! (Z:.nNodes:._) = A.extent adj
        ! results = map (initGraph attr div req) [(i, j) | i <- [0..(nNodes-1)], j <- [(i+1)..(nNodes-1)], adj!(ix2 i j) /= 0]
                      +|| (parBuffer 25 rdeepseq) 
        ! finalGraphs = lefts results
        ! mask = A.fromUnboxed (A.extent adj) $V.replicate (nNodes*nNodes) False V.//
                ((map (\(i,j) -> (i*nNodes + (mod j nNodes), True)) $rights results)
                P.++ (map (\(i,j) -> (j*nNodes + (mod i nNodes), True)) $rights results))
        ! adj' = runIdentity $ A.computeUnboxedP $A.fromFunction (A.extent adj) (\sh -> if mask!sh then 0 else adj!sh)
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
       ! constr = constraintInit attr div req i j
    in case constr of
            Nothing -> Right (i, j)
            Just c  -> Left (A.fromListUnboxed (ix1 2) [i,j], c, 1)

-- | checks constraints of an initializing seed and creates 'Constraints' matrix if the
--   check is positive
constraintInit :: Attr -> MaxDivergence -> Int -- ^ required number of consistent attributes
               -> Int -- ^ first node to test
               -> Int -- ^ second node to test first node against
               -> Maybe Constraints
constraintInit ! attr ! div req i j =
    let
        ! (Z:._:.nAttr) = A.extent attr
        fConstr (Z:.a:.c) =
            let
                ! col = A.slice attr (A.Any:.a)
            in case c of
                    0 -> min (attr!(ix2 i a)) (attr!(ix2 j a))
                    1 -> max (attr!(ix2 i a)) (attr!(ix2 j a))
        (constr, fulfill, nrHit) = runIdentity $
                                        do
                                           ! constr <- return $ A.computeUnboxedS $A.fromFunction (ix2 nAttr 2) fConstr
                                           ! fulfill <- return $ A.computeUnboxedS $ A.zipWith (\thediv dist -> if abs dist <= thediv then 1 else 0) div
                                                                $A.foldS (-) 0 constr
                                           ! nrHit <- return $ A.foldAllS (+) (0::Int) $A.map fromIntegral fulfill
                                           return (constr, fulfill, nrHit)
    in if nrHit >= req then Just (fulfill, constr) else Nothing

-- | removes all duplicate graphs
filterLayer :: [Graph] -> [Graph]
filterLayer gs = L.nubBy filter gs
                where
                        filter :: Graph -> Graph -> Bool
                        filter (ind,_,_) (ind',_,_) = and [V.any (==i) (A.toUnboxed ind) | i <- A.toList ind']

-- | Checks whether a given base 'Graph' can be extended by a single node and
--   the resulting 'Graph' still satisfies the given attribute constraints.
--   In case of a successful expansion the updated 'Constraints' matrix is returned.
constraint :: Attr -> MaxDivergence -> Int -- ^ required number of consistent attributes
           -> Graph -- ^ base graph
           -> Int   -- ^ node to extend base graph by
           -> Maybe Constraints
constraint attr div req (ind, (fulfill, constr), _) newNode =
    let
        --TODO: UGLY hack... this has to be somewhere .. -.-
        ! posInf = read "Infinity" :: Double
        ! negInf = read "-Infinity" :: Double
        -- convert into Vector of new Indices after appending new node-index
        ! totalInd = A.toUnboxed $ A.computeUnboxedS $ ind ++ A.fromListUnboxed (ix1 1) [newNode]
        updateConstr :: (DIM2 -> Double) -> DIM2 -> Double
        updateConstr f sh@(Z:.i:.c) = 
            let
                ! slice = A.slice attr (A.Any :. i)
                -- TODO: why not compare current bounds with attribute values of new node?
                ! mins = A.traverse slice id (\g sh'@(Z :. j)-> if V.any (==j) totalInd then (g sh') else posInf)
                ! maxs = A.traverse slice id (\g sh'@(Z :. j)-> if V.any (==j) totalInd then (g sh') else negInf)
                
            in
            -- trace (show i P.++ show (A.toList slice) P.++ show c P.++ "\n " P.++ show (A.foldAllS (max) negInf $ maxs)) $
            case c of
                 0 -> A.foldAllS (min) posInf mins
                 1 -> A.foldAllS (max) negInf maxs
                 _ -> error "attr wrong"
        ! constrNew = A.computeUnboxedS $ A.traverse constr id updateConstr
        
        --fulfill is borked..
        ! fulfillNew = A.zipWith (\i b -> if i == 1 && b then 1::Int else 0::Int) fulfill
                $A.zipWith (\thediv dist -> abs dist <= thediv) div $A.foldS (-) 0 constrNew
        ! nrHit = A.foldAllS (+) (0::Int) $A.map fromIntegral fulfillNew
    in if nrHit >= req then Just {-$ trace ("returning const-matrix for "P.++ show (A.toList ind) P.++"\n" P.++ (B.unpack $ outputArray constrNew))-}
                                (A.computeUnboxedS fulfillNew, constrNew) else Nothing

-- updates the density of a graph extended by a single node
updateDensity :: Adj            -- ^ global adjacency matrix of all nodes
              -> Vector A.U Int -- ^ nodes of the base graph
              -> Int            -- ^ node to extend the graph by
              -> Density        -- ^ current density of base graph
              -> Density        -- ^ new density of expanded graph
updateDensity adj nodes newNode dens =
    let
    
        
        neighbourSlice = A.map (\n -> fromIntegral $adj!(A.ix2 newNode n)) nodes
                         {-- awefull asymptotic efficiency
                         A.traverse 
                                    (A.slice (A.map fromIntegral adj) (A.Any :. newNode)) -- Array
                                    id                                                    -- same Size
                                    (\f sh@(_ :. i) -> 
                                        if V.any (==i) (A.toUnboxed nodes) then --if connected to graph
                                                (f sh)                          --return connection
                                        else
                                                0)                              --never connect to nodes not extisting
                                    --}
        neighbours = A.foldAllS (+) (0::Int) ({- trace (show $ A.computeUnboxedS neighbourSlice)-} neighbourSlice)
                
                {- A.traverse adj (reduceDim) (\f (Z :. i) -> 
                                if not $ V.any (==i) $ A.toUnboxed nodes then
                                        fromIntegral $adj!(ix2 i newNode) 
                                   else
                                        0)-}
        (Z:.n') = A.extent nodes
        n = fromIntegral n'
        newdens = (dens * ((n)*(n-1)) / 2 + fromIntegral neighbours) * 2 / ((n+1)*(n)) 
    in newdens
        {-+ trace (
                (show dens) P.++ " ("P.++(show (dens * (n*(n-1)) / 2)) P.++"/"P.++ (show ((n*(n-1))/(2::Double))) P.++ ") -> "
                P.++ (show newdens) P.++ " ("P.++(show (newdens * ((n)*(n+1)) / 2)) P.++"/"P.++ (show (((n)*(n+1))/(2::Double))) P.++ ") \n"
                P.++ (show newNode) 
                P.++ " -> " 
                P.++ (show neighbours)) 
                0-}


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
        (constr, densNew) = (constraint attr div req g n,updateDensity adj nodes n dens)
                                 -- +|| (parTuple2 rdeepseq rdeepseq) 
    in
        case densNew >= d of
                False -> Nothing
                True -> 
                        case constr of
                             Nothing  -> Nothing
                             (Just c@(ful,constr)) ->
                                --trace (B.unpack $ outputArray constr) $
                                Just {-$ trace ("submitting graph:\n================\n " P.++ (B.unpack $ outputGraph [(A.computeS $nodes ++ A.fromListUnboxed (ix1 1) [n], c, densNew)])) -}
                                   (A.computeUnboxedS $nodes ++ A.fromListUnboxed (ix1 1) [n], c, densNew)

reduceDim :: (A.Shape sh, Integral a) => (sh :. a) -> sh
reduceDim (a :. b) = a --A.shapeOfList $ tail $ A.listOfShape a

-- | yields all valid addititons (=neighbours) to a Graph
addablePoints :: Adj -> Graph -> Vector A.U Bool
addablePoints adj (ind,_,_) = A.computeUnboxedS $
                                        (A.traverse
                                                adj
                                                reduceDim
                                                (foldOr ind))
           where


                foldOr :: (A.Shape sh') => Vector A.U Int -> ((sh' :. Int :. Int) -> Int8) -> (sh' :. Int) -> Bool
                foldOr indlist lookup ind@(a :. pos) = case V.any (== pos) $ A.toUnboxed indlist of
                                                True -> False
                                                _ -> (foldl1 (+) [lookup (ind :. i) | i <- (map fromIntegral (A.toList indlist))]) > 0


