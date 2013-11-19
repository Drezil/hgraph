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
import Data.List
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
import Stream hiding (map)
import qualified Data.Array.Accelerate as A
-- change to Data.Array.Accelerate.CUDA as I and link accelerate-cuda to use GPU instead of CPU
-- depends on accelerate-cuda package in cabal, which needs the installed CUDA-stuff form
-- nVidia (nvcc, header-files, ...) and the propriatary driver
import Data.Array.Accelerate.Interpreter as I
type Matrix e = A.Array A.DIM2 e


-- TODO: Give createGraph a presized Array and no dynamic [Int].
-- should be createGraph :: T.Text -> Either (Vector Int) T.Text
createGraph :: T.Text -> Either [Int] T.Text
createGraph input = createGraph' input (Left [])
    where
        createGraph' :: T.Text -> Either [Int] T.Text -> Either [Int] T.Text
        createGraph' a r
            | T.null a = r
            | otherwise =
                    let next = (createGraph' (T.tail a) r) in
                        case next of
                            Left xs ->
                                case T.head (traceEvent "parsing" a) of
                                    '0' -> Left $ traceEvent "parse-concat" 0:xs
                                    '1' -> Left $ traceEvent "parse-concat" 1:xs
                                    _   -> Right $ T.append (T.pack "cannot parse ") a
                            Right errstr ->
                                Right errstr

emptyLine :: T.Text -> Bool
emptyLine a
    | T.null a    = True
    | otherwise   = False

emptyLog :: [T.Text] -> Bool
emptyLog [] = True
emptyLog a = False --emptyLine $ foldl True (&&) (map emptyLine a)

-- TODO: implement calculation
--doCalculation :: Matrix Int -> B.ByteString
doCalculation a = B.pack $ (show a) ++ "\n"

infixl 1 +||

(+||) :: a -> Strategy a -> a
a +|| b = a `using` b

exeMain = do
    args <- getArgs
    input <- case args of
            ["-"] -> B.getContents
            [] -> error "Error: No filename or stdinput (-) given."
            [file] -> B.readFile file
    -- read file and clean
    readFile <- return $ filter (not . emptyLine) (T.lines (decodeUtf8 input))
    inputLines <- return $ length readFile
    -- TODO: concat with foldl1' kills us later -> use presized/preallocated array so we
    --       dont copy that much lateron. Best would be Matrix Int
    -- unrefined_graph::[Either [Int] String] - [Int] is Adjacency-Line, String is parse-Error
    unrefined_graph <- return $ (map (traceEvent "mapping" . createGraph) readFile)
                                        +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    --egraph <- return $ graphFolder unrefined_graph

    (graph, log, lines) <- return $ ((foldl1' ((traceEvent "concatenating graph") . (++)) (lefts unrefined_graph), -- concatenated graph
                                traceEvent "concatenating log" T.intercalate (T.singleton '\n') (rights unrefined_graph), -- concat error-log
                                traceEvent "getting length" length unrefined_graph) -- number of elements in graph
                                                    -- in parallel
                                                    `using` parTuple3 rdeepseq rdeepseq rdeepseq)

    -- validate graph
    log <- return $ let l = traceEvent "first validation" length graph in
                        if l /= lines*lines then
                            T.append log $ T.pack $ "Lines dont match up. Read " ++ (show l) ++
                                                    " chars. Expected " ++ (show (lines*lines)) ++
                                                    " chars.\n"
                        else
                            log
    output <- return $ case emptyLine (traceEvent "last validation" log) of
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
