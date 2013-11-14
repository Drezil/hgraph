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
import Data.List
import System.Exit (exitFailure)
import System.Environment
import Test.QuickCheck.All (quickCheckAll)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad.Par.Scheds.Trace
import qualified Data.Stream as S
import Data.Either (lefts, rights)

import Stream


-- TODO: implement parser!
createGraph :: ByteString -> Either [Int] String
createGraph _ = Left [1,2,3]

graphFolder :: [Either [Int] String] -> (Either [[Int]] String)
graphFolder [] = Right "empty Graph"
graphFolder l = graphFolder' l (Left [[]])
    where
    graphFolder' :: [Either [Int] String] -> (Either [[Int]] String) -> (Either [[Int]] String)
    graphFolder' [] r      = r
    graphFolder' (a:as) r  =
                    case a of
                    -- we have an intact [Int]
                    Left b ->
                        case graphFolder' as r of
                        -- append if rest is ok.
                        Left xs  -> Left (b:xs)
                        -- ooops. Error-String -> Discard result
                        Right s -> Right s
                    -- we have an Error-String -> ignore results, append errors if possible
                    Right s ->
                        case graphFolder' as r of
                        Left x   -> Right s
                        Right ss -> Right (ss ++ "\n" ++ s)

concatWith :: String -> String -> String -> String
concatWith d a b = a ++ d ++ b

exeMain = do
    args <- getArgs
    input <- case args of
            ["-"] -> B.getContents
            [] -> error "Error: No filename or stdinput (-) given."
            [file] -> B.readFile file
    -- unrefined_graph::[Either [Int] String] - [Int] is Adjacency-Line, String is parse-Error
    unrefined_graph <- return $ parMap (rparWith rdeepseq) (createGraph) (B.split '\n' input)
    --egraph <- return $ graphFolder unrefined_graph
    (graph, log) <- return (lefts unrefined_graph, rights unrefined_graph)

    --do stuff with graph

    B.putStr $ B.pack (foldl (concatWith "\n") "" log) -- Print output



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
