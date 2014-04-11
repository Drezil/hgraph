{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Functions to transform graphs and DCBs into text.
module DCB.IO where

import           Control.Parallel.Strategies
import           Data.Array.Repa              as A hiding ((++))
import           Data.Array.Repa.Repr.Unboxed
import           Data.Array.Repa.Repr.Vector
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B
import qualified Data.List                    as L
import qualified Data.Vector.Unboxed          as V
import           DCB.Structures
import           Util



-- | creates a default-formatted output with @,@ in between elements
--   and @\\n@ in between dimensions
--
--   calls '_outputArray' with preset properties
outputArray :: (Unbox a, Show a) => Array U DIM2 a -> B.ByteString
outputArray a = _outputArray a "\t" "\n"

-- | creates a formatted output from a 'DIM2' repa-'Data.Array.Repa.Array'
--
--   * First String is the between-element-separator
--
--   * Second String is the between-dimensions-separator
_outputArray :: (Unbox a, Show a) => Array U DIM2 a -> String -> String -> B.ByteString
_outputArray a itt lt = B.concat $
                                (B.pack $ "Matrix "++(show $ listOfShape $ extent a)++ "\n")
                                : (L.map B.pack (_outputArray' (extent a) a itt lt))
        where
        _outputArray' :: (Unbox a, Show a) => DIM2 -> Array U DIM2 a -> String -> String -> [String]
        _outputArray' shape@(Z :. si :. sj) a itt lt = [(_outputArray'' shape i 0 a itt) ++ lt | i <- [0..(si - 1)]]

        _outputArray'' :: (Unbox a, Show a) => DIM2 -> Int -> Int -> Array U DIM2 a -> String -> String
        _outputArray'' shape@(Z :. si :. sj) i j a itt
                        | sj-1 == j = show (a!(ix2 i j))  -- no "," for last one..
                        | otherwise = show (a!(ix2 i j)) ++ itt ++ (_outputArray'' shape i (j+1) a itt)

-- | creates a default-formatted output with @,@ in between elements
--   and @\\n@ in between dimensions
--
--   calls '_outputArray' with preset properties
outputGraph :: [Graph] -> B.ByteString
outputGraph gs = B.concat $ L.map (flipto3 _outputGraph "," "\n") (L.sort gs)
                                        +|| (parBuffer 25 rseq) --run parallel

-- | creates a formatted output from a Graph
--
--   * First String is the between-element-separator
--
--   * Second String is the between-dimensions-separator
--
--   Example Output with @,@ and @\\n@:
--
--   > Density: 
--   > 0.7619047619047619
--   > Indices used:
--   > 28,71,78,81,100,349,401,
--   > Attribute-Dimensions satisfied:
--   > 0,0,1,0,1,1,
--   > Matrix [6,2]
--   > 28.0    3.0     1.0     551.0   0.0     10.0
--   > 401.0   67.0    4.0     2524.0  5.0     19.0
--
_outputGraph :: Graph -> String -> String -> B.ByteString
_outputGraph (indices, (constdim, constmat), dens) itt lt =
                                    let
                                            plt = B.pack lt
                                            pitt = B.pack itt
                                    in
                                        B.concat $
                                        [
                                                (B.pack $ "Density: " ++ lt ++ show dens),
                                                plt,
                                                (B.pack $ "Indices used:" ++ lt ++ V.foldl (appendS itt) "" (toUnboxed indices)),
                                                plt,
                                                (B.pack $ "Attribute-Dimensions satisfied:" ++ lt ++ V.foldl (appendS itt) "" (toUnboxed constdim)),
                                                plt,
                                                outputArray $ computeS $ transpose constmat,
                                                plt,
                                                plt
                                        ]

