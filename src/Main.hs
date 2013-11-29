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
import qualified Data.List                      as L
import qualified Data.Stream                    as S
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Debug.Trace
import           System.Environment
import           System.Exit                    (exitFailure)
import           Test.QuickCheck.All            (quickCheckAll)
import Data.Functor.Identity


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
doCalculation graph attr = createOutput graph


createOutput :: (Unbox a, Show a) => Array U DIM2 a -> B.ByteString
createOutput a = B.concat $ L.map B.pack (createOutput' (extent a) a)

createOutput' :: (Unbox a, Show a) => DIM2 -> Array U DIM2 a -> [String]
createOutput' shape@(Z :. si :. sj) a = [(createOutput'' shape i 0 a) ++ "\n" | i <- [0..(si - 1)]]

createOutput'' :: (Unbox a, Show a) => DIM2 -> Int -> Int -> Array U DIM2 a -> String
createOutput'' shape@(Z :. si :. sj) i j a 
                        | sj-1 == j = show (a!(ix2 i j))  -- no "," for last one..
                        | otherwise = show (a!(ix2 i j)) ++ "," ++ (createOutput'' shape i (j+1) a)    
                                   
{-
T.intercalate (T.singleton ',') (L.map (T.pack . show) a)
createOutput' (a:as) = T.append
                                    (T.append
                                            (T.intercalate (T.singleton ',')
                                            (L.map (T.pack . show) a))
                                     (T.singleton '\n'))
                               (createOutput' as)
-}

getAttrLength :: Either [Double] T.Text -> Int
getAttrLength (Left a) = length a
getAttrLength (Right _) = 0

showHelp = undefined

infixl 1 +||

(+||) :: a -> Strategy a -> a
a +|| b = a `using` b

checkError :: T.Text -> IO ()
checkError a
        | T.null a  = return ()
        | otherwise = do
                        B.putStr $ encodeUtf8 $ T.append (T.append (T.pack "Error detected:\n") a) (T.pack "\n\n")
                        exitFailure

exeMain = do
--    args <- getArgs
--    input <- case args of
--            ["--help"] -> showHelp
--            ["-h"] -> showHelp
--            [] -> error "Error: No filename or stdinput (-) given."
--            [adj, attr] -> Prelude.mapM B.readFile [adj, attr]
--            _ -> error "Wrong arguments given"
    input <- Prelude.mapM B.readFile ["sampledata.adj","sampledata.adj.atr"]
    -- read file and clean
    adjMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 (head input)))
    attrMat <- return $ L.filter (not . emptyLine) (T.lines (decodeUtf8 ((head . L.tail) input)))

    adjLines <- return $ length adjMat
    attrLines <- return $ length attrMat
    -- TODO: concat with foldl1' kills us later -> use presized/preallocated array so we
    --       dont copy that much lateron. Best would be Matrix Int
    -- unrefined_graph::[Either [Int] String] - [Int] is Adjacency-Line, String is parse-Error
    unrefined_graph <- return $ (L.map (createGraph) adjMat)
                                      --  +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    unrefined_attr <- return $ (L.map (createAttr) attrMat)
                                      --  +|| (parBuffer 100 rdeepseq) --run parallel, evaluate fully
    attrNum <- return $ getAttrLength (head unrefined_attr)
    putStrLn $ show (adjLines, attrLines, attrNum)
    
    ----- CHECK FOR ERRORS
    -- print out any read-errors and abort
    if adjLines /= attrLines then 
        checkError $ T.pack $ "Adjacency-Matrix size "++ show adjLines ++
                              " differs from Attribute-Matrix " ++ show attrLines ++
                              ".\n"
    else
        return ()    
    checkError $ T.intercalate (T.singleton '\n') (rights unrefined_graph)
    checkError $ T.intercalate (T.singleton '\n') (rights unrefined_attr)

    putStrLn $ show (length (L.foldl1 (++) (lefts unrefined_graph)),length (L.foldl1 (++) (lefts unrefined_attr)))
    ----- EXTRACT MATRICES
    graph <- return $ A.fromListUnboxed (Z :. adjLines :. adjLines) (L.foldl1' (++) (lefts unrefined_graph)) -- concatenated graph

    attr <- return $ A.fromListUnboxed (Z :. attrLines :. attrNum) (L.foldl1' (++) (lefts unrefined_attr)) -- concatenated attr
    
    ----- CALCULATE
    output <- return $ doCalculation graph attr
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
