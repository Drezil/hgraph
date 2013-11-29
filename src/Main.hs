{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           DCB

import           Control.DeepSeq
import           Control.Monad                  (unless)
import           Control.Monad.Par.Scheds.Trace
import           Control.Parallel.Strategies
import           Data.Array.Repa                as A hiding ((++))
import           Data.Array.Repa.Repr.Unboxed
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Either                    (lefts, rights)
import           Data.Functor.Identity
import qualified Data.List                      as L
import qualified Data.Stream                    as S
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Debug.Trace
import           System.Environment
import           System.Exit                    (exitFailure, exitSuccess)
import           Test.QuickCheck.All            (quickCheckAll)


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
        createAttr' [] r     = r
        createAttr' (a:as) r =
                    let this = read (T.unpack a) :: Double in
                        (if isNaN this then
                                Right $ T.append (T.pack "cannot parse ") a
                         else
                           (let next = (createAttr' as r) in
                              case next of
                                  Left rs -> Left (this : rs)
                                  _ -> next))

emptyLine :: T.Text -> Bool
emptyLine a
    | T.null a    = True
    | otherwise   = False

emptyLog :: [T.Text] -> Bool
emptyLog [] = True
emptyLog a = False --emptyLine $ foldl True (&&) (map emptyLine a)

-- TODO: implement calculation
--doCalculation :: Matrix Int -> B.ByteString
doCalculation graph attr = createOutput attr

-- | creates a default-formatted output with "," in between elements
--   and "\n" in between dimensions
--   calls just _createOutput with preset properties

--default output with "," within items and "\n" within dimensions
createOutput :: (Unbox a, Show a) => Array U DIM2 a -> B.ByteString
createOutput a = _createOutput a "," "\n"

-- | creates a formatted output from a DIM2 repa-Array

--output Array a with "itt" within items and "lt" within dimensions
_createOutput :: (Unbox a, Show a) => Array U DIM2 a -> String -> String -> B.ByteString
_createOutput a itt lt = B.concat $ L.map B.pack (_createOutput' (extent a) a itt lt)

_createOutput' :: (Unbox a, Show a) => DIM2 -> Array U DIM2 a -> String -> String -> [String]
_createOutput' shape@(Z :. si :. sj) a itt lt = [(_createOutput'' shape i 0 a itt) ++ lt | i <- [0..(si - 1)]]

_createOutput'' :: (Unbox a, Show a) => DIM2 -> Int -> Int -> Array U DIM2 a -> String -> String
_createOutput'' shape@(Z :. si :. sj) i j a itt
                        | sj-1 == j = show (a!(ix2 i j))  -- no "," for last one..
                        | otherwise = show (a!(ix2 i j)) ++ itt ++ (_createOutput'' shape i (j+1) a itt)


-- | gets the length of the Left a.
--   0 if Left a empty or no valid constructor.
getLength :: Either [a] T.Text -> Int
getLength (Left a) = length a
getLength (Right _) = 0


-- | prints the Help and exits
showHelp :: IO ()
showHelp = do
                putStrLn $ "Usage: hgraph <adjacency> <attribute>\n"++
                           "\n" ++
                           "-h         show help\n" ++
                           "--help\n" ++
                           "\n" ++
                           "adjacency  An adjecency-Matrix with 0 or 1 as weights for edges\n"++
                           "           seperated by newlines for each row.\n"++
                           "           Must be NxN.\n"++
                           "\n"++
                           "attribute  A tabulator-seperated Matrix of attributes.\n" ++
                           "           Must be Nxk.\n"++
                           "\n"
                exitSuccess

infixl 1 +||

-- | short for a `using` b. We don't need brackets this way and are able to comment out parallelism.
(+||) :: a -> Strategy a -> a
a +|| b = a `using` b


-- | checks if the submitted Text is empty. If not it will be printed out and the program aborts
checkError :: T.Text -> IO ()
checkError a
        | T.null a  = return ()
        | otherwise = do
                        B.putStr $ encodeUtf8 $ T.append (T.append (T.pack "Error detected:\n") a) (T.pack "\n\n")
                        exitFailure

--change Debug to return () lateron.
debug a = putStrLn a

exeMain = do
--    args <- getArgs
--    input <- case args of
--            ["--help"] -> showHelp
--            ["-h"] -> showHelp
--            [] -> error "Error: Wrong number of Arguments given. Try --help for more information."
--            [adj, attr] -> Prelude.mapM B.readFile [adj, attr]
--            _ -> error "Wrong arguments given"
    input <- Prelude.mapM B.readFile ["sampledata.adj","sampledata.adj.atr"]
    -- read file and clean
    adjMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 (head input)))
    attrMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 ((head . L.tail) input)))

    adjLines <- return $ length adjMat
    attrLines <- return $ length attrMat

    unrefined_graph <- return $ (L.map (createGraph) adjMat)
                                        +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    unrefined_attr <- return $ (L.map (createAttr) attrMat)
                                        +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    adjNum <- return $ getLength (head unrefined_graph)
    attrNum <- return $ getLength (head unrefined_attr)
    debug $ show (adjLines, attrLines, attrNum)

    ----- CHECK FOR ERRORS
    ---- print out any read-errors and abort
    -- parser-errors
    checkError $ T.intercalate (T.singleton '\n') (rights unrefined_graph)
    checkError $ T.intercalate (T.singleton '\n') (rights unrefined_attr)
    -- attribute-errors
    if adjLines /= attrLines then
        checkError $ T.pack $ "Adjacency-Matrix size "++ show adjLines ++
                              " differs from Attribute-Matrix " ++ show attrLines ++
                              ".\n"
    else
        return ()
    if adjLines /= adjNum then
        checkError $ T.pack $ "Adjacency-Matrix is not square.\n" ++
                              "Read format is " ++ show adjNum ++
                              "x" ++ show attrNum ++ ".\n"
    else
        return ()

    ----- EXTRACT MATRICES
    graph <- return $ A.fromListUnboxed (Z :. adjLines :. adjLines) (L.foldl1 (++) (lefts unrefined_graph)) -- concatenated graph

    attr <- return $ A.fromListUnboxed (Z :. attrLines :. attrNum) (L.foldl1 (++) (lefts unrefined_attr)) -- concatenated attr

    ----- CALCULATE & OUTPUT

    B.putStr $ doCalculation graph attr


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
