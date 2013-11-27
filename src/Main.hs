{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Control.Monad (unless)
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.List as L
import System.Exit (exitFailure)
import System.Environment
import Test.QuickCheck.All (quickCheckAll)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Par.Scheds.Trace
import qualified Data.Stream as S
import Data.Either (lefts, rights)
import Debug.Trace
import qualified Data.Text as T
import Data.Text.Encoding
--import Stream hiding (map) --same as Data.Stream imported above?
import Data.Array.Accelerate hiding (not,(++))
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


-- TODO: Give createGraph a presized Array and no dynamic [Int].
-- should be createGraph :: T.Text -> Either (Vector Int) T.Text
createGraph :: T.Text -> Either [Int] T.Text
createGraph input = createGraph' input (Left [])
    where
        createGraph' :: T.Text -> Either [Int] T.Text -> Either [Int] T.Text
        createGraph' a r
            | T.null a = r
            | otherwise =
                    let next = (createGraph' (T.tail a) r) in        -- flip cases for less function-calls?
                        case next of
                            Left xs ->
                                case T.head a of
                                    '0' -> Left $ 0:xs
                                    '1' -> Left $ 1:xs
                                    _   -> Right $ T.append (T.pack "cannot parse ") a
                            Right errstr ->
                                Right errstr

createAttr :: T.Text -> Either [Double] T.Text
createAttr input = createAttr' (T.split (=='\t') input) (Left [])
    where
        createAttr' :: [T.Text] -> Either [Double] T.Text -> Either [Double] T.Text
        createAttr' (a:as) r =
                    let this = read (T.unpack a) :: Double in
                        case isNaN this of
                            True -> Right $ T.append (T.pack "cannot parse ") a
                            _ ->
                                let next = (createAttr' as r) in
                                  case next of
                                    Left rs -> Left (this:rs)
                                    _ -> next

emptyLine :: T.Text -> Bool
emptyLine a
    | T.null a    = True
    | otherwise   = False

emptyLog :: [T.Text] -> Bool
emptyLog [] = True
emptyLog a = False --emptyLine $ foldl True (&&) (map emptyLine a)

-- TODO: implement calculation
--doCalculation :: Matrix Int -> B.ByteString
doCalculation a = B.pack $ ""--(show a) ++ "\n"


createOutput :: [[Int]] -> B.ByteString
createOutput a = encodeUtf8 (createOutput' a)

createOutput' :: [[Int]] -> T.Text
createOutput' [a] = T.intercalate (T.singleton ',') (L.map (T.pack . show) a)
createOutput' (a:as) = T.append
                                    (T.append
                                            (T.intercalate (T.singleton ',')
                                            (L.map (T.pack . show) a))
                                     (T.singleton '\n'))
                               (createOutput' as)

-- preprocess ::

showHelp = undefined

infixl 1 +||

(+||) :: a -> Strategy a -> a
a +|| b = a `using` b

exeMain = do
    args <- getArgs
    input <- case args of
            ["--help"] -> showHelp
            ["-h"] -> showHelp
            [] -> error "Error: No filename or stdinput (-) given."
            [adj, attr] -> Prelude.mapM B.readFile [adj, attr]
            _ -> error "Wrong arguments given"
    -- read file and clean
    adjMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 (head input)))
    attrMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 ((head . L.tail) input)))

    adjLines <- return $ length adjMat
    attrLines <- return $ length attrMat
    -- TODO: concat with foldl1' kills us later -> use presized/preallocated array so we
    --       dont copy that much lateron. Best would be Matrix Int
    -- unrefined_graph::[Either [Int] String] - [Int] is Adjacency-Line, String is parse-Error
    unrefined_graph <- return $ (L.map (createGraph) adjMat)
                                        +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    unrefined_attr <- return $ (L.map (createAttr) attrMat)
                                        +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    --egraph <- return $ graphFolder unrefined_graph

    (graph, log, lines) <- return $ ((L.foldl1' (++) (lefts unrefined_graph), -- concatenated graph
                                T.intercalate (T.singleton '\n') (rights unrefined_graph), -- concat error-log
                                length unrefined_graph) -- number of elements in graph
                                                    -- in parallel
                                                    `using` parTuple3 rdeepseq rdeepseq rseq)

    (attr, log, linesAttr) <- return $ ((L.foldl1' (++) (lefts unrefined_graph), -- concatenated graph
                                T.append log (T.intercalate (T.singleton '\n') (rights unrefined_graph)), -- concat error-log
                                length unrefined_graph) -- number of elements in graph
                                                    -- in parallel
                                                    `using` parTuple3 rdeepseq rdeepseq rseq)

    -- validate graph
    log <- return $ let l = length graph in
                        if l /= lines*lines then
                            T.append log $ T.pack $ "Lines dont match up. Read " ++ (show l) ++
                                                    " chars. Expected " ++ (show (lines*lines)) ++
                                                    " chars.\n"
                        else if adjLines /= attrLines then
                            T.append log $ T.pack $ "Adjecency-Matrix size "++ (show adjLines) ++
                                                    " differs from Attribute-Matrix " ++ (show attrLines) ++
                                                    ".\n"
                        else
                            log
    output <- return $ case emptyLine log of
        True -> doCalculation $ graph --A.fromList (A.Z A.:. lines A.:. lines) graph
        _    -> encodeUtf8 $ T.append (T.append (T.pack "Error detected:\n") log) (T.pack "\n\n")
    B.putStr output


-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION
