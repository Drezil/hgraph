{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
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

module Main where

import           DCB.DCB
import           DCB.IO
import           Util

import           Control.DeepSeq
import           Control.Monad                  (unless)
import           Control.Monad.Par.Scheds.Trace
import           Control.Parallel.Strategies
import           Data.Array.Repa                as A hiding ((++))
import           Data.Array.Repa.Eval           (Elt)
import           Data.Array.Repa.Repr.Unboxed
import           Data.Array.Repa.Repr.Vector
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Char                      (isSpace)
import           Data.Either                    (lefts, rights)
import           Data.Functor.Identity
import           Data.Int
import qualified Data.List                      as L
import qualified Data.Stream                    as S
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Data.Vector.Unboxed            as V
import           Debug.Trace
import           System.Environment
import           System.Exit                    (exitFailure, exitSuccess)
import           Test.QuickCheck.All            (quickCheckAll)


-- | Parses the graph
--   a graph consists of NxN chars layouted like
--
--  > 10101
--  > 01010
--  > 00100
--  > 01010
--  > 10101
--
--    * Valid Chars: 0, 1, \\n
--
--    * Invalid: \\r

createGraph :: (Elt a, Integral a) => T.Text -> Either [a] T.Text
createGraph (!input) = createGraph' input (Left [])
    where
        createGraph' :: (Elt a, Integral a) => T.Text -> Either [a] T.Text -> Either [a] T.Text
        createGraph' a r
            | T.null a = r
            | otherwise =
                    case T.head a of
                        '0' -> createGraph'' 0 (T.tail a) r
                        '1' -> createGraph'' 1 (T.tail a) r
                        _   -> Right $ T.append (T.pack "cannot parse ") a
                        -- call recursion as last resort -> ensure not much happens on the heap
                        where
                            createGraph'' :: (Elt a, Integral a) => a -> T.Text -> Either [a] T.Text -> Either [a] T.Text
                            createGraph'' x cs r =
                                case createGraph' cs r of
                                    Left xs -> Left (x:xs)
                                    Right errstr ->
                                        Right errstr

-- | Parses the attribute-Matrix
--   the matrix consists of NxM tab-delimeted double-lines like
--
--  > 1     2.3
--  > -1    2.1
--  > 4     2.7
--  > 2.2   -3e-4
--  > 3     2.3
--
--    * Valid: Doubles, Tabs (\\t)
--

createAttr :: T.Text -> Either [Double] T.Text
createAttr (!input) = createAttr' (T.split (=='\t') input) (Left [])
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

-- | checks if a given Text is empty (Empty String, whitespaces)
emptyLine :: T.Text -> Bool
emptyLine a
    | T.null a        = True
    | T.all isSpace a = True
    | otherwise       = False

-- TODO: implement calculation
--doCalculation :: Matrix Int -> B.ByteString
doCalculation adj attr =
        let
                dens = 0.75
                omega = (A.fromListUnboxed (ix1 3) [0.5,0.5,0.5])
                delta = 2
                (adj_, graph_) = preprocess adj attr {--0.8--} omega delta
        in
                B.concat $
                        [
                                outputArray $ trace ("After: "++ show (sumAllS adj_)++"\n") adj_,
                                outputGraph $ L.sort $ doAll graph_ adj_ attr dens omega delta
--                                outputGraph $ L.sort $ (step graph_ adj attr dens omega delta)
--                                                       ++ (step (step graph_ adj attr dens omega delta) adj attr dens omega delta)
                        ]
                where
                        doAll [] _ _ _ _ _ = []
                        doAll gs a b c d e = doAll' (step gs a b c d e) a b c d e
                        doAll' [] _ _ _ _ _ = []
                        doAll' gs a b c d e = gs ++ doAll' (step gs a b c d e) a b c d e

-- | gets the length of the Left a.
--
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

-- | checks if the submitted Text is empty. If not it will be printed out and the program aborts
checkError :: T.Text -> IO ()
checkError a
        | emptyLine a  = return ()
        | otherwise = do
                        B.putStr $ encodeUtf8 $ T.append (T.append (T.pack "Error detected:\n") a) (T.pack "\n\n")
                        exitFailure

-- | convinience debug-function. Needs to be
--   changed to return () to disable Debug.
debug a = putStrLn a


-- | The main-function to bootstrap our application
main = do
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
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
    unrefined_attr <- return $ (L.map (createAttr) attrMat)
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
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

    debug $ "Before: " ++ show (sumAllS graph)
    B.putStr $ doCalculation graph attr
