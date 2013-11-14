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

import Stream hiding (map)


-- TODO: implement parser!
createGraph :: String -> Either [Int] String
createGraph input = createGraph' input (Left [])
    where
        createGraph' :: String -> Either [Int] String -> Either [Int] String
        createGraph' [] r     = r
        createGraph' (a:as) r =
                    let next = (createGraph' as r) in
                        case next of
                            Left xs ->
                                case a of
                                    '0' -> Left $ 0:xs
                                    '1' -> Left $ 1:xs
                                    _   -> Right $ "cannot parse " ++ (a:as)
                            Right errstr ->
                                Right errstr
--createGraph input = Right $ "Parsing-error in line: " ++ input

-- TODO: not needed anymore. remove? Later use?
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

emptyLine :: String -> Bool
emptyLine "" = True
emptyLine "\n" = True
emptyLine "\r\n" = True
emptyLine "\r" = True
emptyLine _ = False

emptyLog :: [String] -> Bool
emptyLog [] = True
emptyLog a = emptyLine $ foldl1 (++) a

-- TODO: implement calculation
doCalculation :: [[Int]] -> ByteString
doCalculation a = B.pack $ (show a) ++ "\n"

exeMain = do
    args <- getArgs
    input <- case args of
            ["-"] -> B.getContents
            [] -> error "Error: No filename or stdinput (-) given."
            [file] -> B.readFile file
    -- unrefined_graph::[Either [Int] String] - [Int] is Adjacency-Line, String is parse-Error
    unrefined_graph <- return $ parMap (rparWith rdeepseq) --run parallel, evaluate fully
                                                        -- and filter empty lines
                                        (createGraph) (filter (not . emptyLine)
                                                        -- split at \n, convert to String
                                                        (map B.unpack (B.split '\n' input)))
    --egraph <- return $ graphFolder unrefined_graph
    (graph, log) <- return (lefts unrefined_graph, rights unrefined_graph)
    output <- return $ case emptyLog log of
        True -> doCalculation graph
        _    -> B.pack $ "Error detected:\n" ++ (foldl (concatWith "\n") "" log) ++ "\n\n"
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
