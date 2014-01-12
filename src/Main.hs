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
import           DCB.Structures
import           DCB.IO
import           Util

import           Control.DeepSeq
import           Control.Monad                  (unless)
import           Control.Monad.Par.Scheds.Trace
import           Control.Parallel.Strategies
import qualified Data.Array.Repa                as A hiding ((++))
import           Data.Array.Repa.Index
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
import qualified Data.Vector.Unboxed            as V
--import           Debug.Trace
import           System.CPUTime
import           System.Environment
import           System.Exit                    (exitFailure, exitSuccess)
--import           Test.QuickCheck.All            (quickCheckAll)


data Params = Params { density :: Double
                     , matches :: Int
                     , range   :: [Double]
                     } deriving (Show,Eq)

instance NFData Params

-- What could be improved: In case of an error the full ByteString is reduced nonetheless because of
-- the use of fold. It would be better to be able to skip the parsing when an error occurs.
-- | Parses a row of the adjacency matrix of a graph. The consistancy of line lengths is not tested 
--   by this function! In case of a successfull parse a 'Left [a]' is returned, otherwise a
--   'Right ByteString' containing an error message.
--   > 10101
--   > 01010
--   > 00100
--   > 01010
--   > 10101
--   
--    * whitespace in between is ignored (including '\\t', '\\n' and '\\r')
--    * Valid Values: '0', '1'
--    * any invalid value which is not a whitespace character raises an error
parseAdjMat :: (Integral a) => ByteString -> Either [a] ByteString
parseAdjMat input = B.foldr' foldf (Left []) input -- important to use *right* fold to keep ordering
    where --foldf :: Char -> Either [a] ByteString -> Either [a] ByteString
          foldf _ (r@(Right _)) = r
          foldf c (l@(Left row))
            | c == '0'  = Left (0:row)
            | c == '1'  = Left (1:row)
            | isSpace c = l
            | otherwise = Right (B.pack $ "(adjacency)cannot parse '" ++ c:"'")

testParse :: [(a,String)] -> Maybe a
testParse [] = Nothing
testParse [(a,s)] = if isWhitespace s then Just a else Nothing
testParse _  = Nothing

-- What could be improved: In case of an error the full ByteString is reduced nonetheless because of
-- the use of fold. It would be better to be able to skip the parsing when an error occurs.
-- | Parses a row of the attribute matrix of a graph. The consistancy of line lengths is not tested 
--   by this function! In case of a successfull parse a 'Left [a]' is returned, otherwise a
--   'Right ByteString' containing an error message.
--   > 1     2.3
--   > -1    2.1
--   > 4     2.7
--   > 2.2   -3e-4
--   > 3     2.3
--
--   * Valid: Doubles
--   * whitespace between two values is ignored
--   * any invalid value which is not a whitespace character raises an error
parseAttr :: Char -> ByteString -> Either [Double] ByteString
parseAttr delim input = parseAttr' (B.split delim input)
    where parseAttr' :: [ByteString] -> Either [Double] ByteString
          parseAttr' [] = Left []
          parseAttr' (t:ts) =
              case testParse (reads (B.unpack t) :: [(Double, String)]) of
                   Just d -> case isNaN d of
                                  False -> case parseAttr' ts of
                                                Left fs   -> Left (d:fs)
                                                Right msg -> Right msg
                                  True  -> Right $ B.pack ("(attr)invalid value " ++ show d)
                   _      -> Right $ B.append (B.pack "(attr)cannot parse ") t

parseParams :: Char -> [ByteString] -> Either Params ByteString
parseParams delim input
  | length input /= 3 = Right $ B.pack ("(params)amount of lines does not match (expected 3, got "
                                ++ show (length input) ++ ")")
  | otherwise =
    case testParse ((reads . B.unpack) (head input) :: [(Double, String)]) of -- parse density
         Just dens -> case testParse ((reads . B.unpack) (head $ tail input) :: [(Int, String)]) of -- parse matches
                           Just match -> case parseAttr delim (head $ tail $ tail input) of --parse range line
                                              Left range -> Left $ Params dens match range
                                              Right msg  -> Right $ B.append (B.pack "(param - range)") msg
                           _        -> Right $ B.append (B.pack "(param - match)cannot parse ") (head $ tail input)
         _         -> Right $ B.append (B.pack "(param - density)cannot parse ") (head input)


-- | checks if a given Text is empty (Empty String, whitespaces)
emptyLine :: ByteString -> Bool
emptyLine a
    | B.null a        = True
    | B.all isSpace a = True
    | otherwise       = False

-- | Starts the calculation of a given DCB problem.
doCalculation :: Adj -> Attr -> Params -> B.ByteString
doCalculation adj attr p =
        let
                dens = density p --0.75
                nAttr = length (range p)
                omega = A.fromListUnboxed (ix1 nAttr) (range p) --(A.fromListUnboxed (ix1 6) [0,5,3,300,5,10])
                delta = matches p --2
                (adj_, graph_) = preprocess adj attr {--0.8--} omega delta
        in
                B.concat $
                        [
                                --outputArray $ --trace ("After: "++ show (sumAllS adj_)++"\n") 
                                --              adj_,
                                outputGraph $ L.sort $ doAll graph_ adj_ attr dens omega delta
                        ]
                where
                        -- don't print out seeds
                        doAll [] _ _ _ _ _ = []
                        doAll gs a b c d e = doAll' (step gs a b c d e) a b c d e
                        -- but everything in the following recursive calls
                        doAll' [] _ _ _ _ _ = []
                        doAll' gs a b c d e = gs ++ doAll' (step gs a b c d e) a b c d e

-- | gets the length of the Left a.
--
--   0 if Left a empty or no valid constructor.
getLength :: Either [a] b -> Int
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
checkError :: ByteString -> IO ()
checkError a
        | emptyLine a  = return ()
        | otherwise = do
                        B.putStr $ B.append (B.append (B.pack "Error detected:\n") a) (B.pack "\n\n")
                        exitFailure

-- | convinience debug-function. Needs to be
--   changed to return () to disable Debug.
debug a = return () --putStrLn a


-- | The main-function to bootstrap our application
main = do
    timeStartProg <- getCPUTime
    args <- getArgs
    input <- case args of
            [] -> Prelude.mapM B.readFile ["sampledata.adj","sampledata.adj.atr","sampledata.p"] 
--            ["--help"] -> showHelp -- TODO: implement help display
--            ["-h"] -> showHelp
            [adj, attr, params] -> Prelude.mapM B.readFile [adj, attr, params]
            _ -> error "Error: Wrong number of Arguments given. Try --help for more information."

    -- read file and clean
    adjMat <- return $ L.filter (not . emptyLine) (B.lines (head input))
    attrMat <- return $ L.filter (not . emptyLine) (B.lines (head $ tail input))
    paramRef <- return $ L.filter (not . emptyLine) (B.lines (head $ tail $ tail input))
   

    unrefined_graph <- return $!! (L.map (parseAdjMat) adjMat)
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
    unrefined_attr <- return $!! (L.map (parseAttr '\t') attrMat)
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
    paramsParsed <- return $ parseParams '\t' paramRef
    
    adjLines <- return $ length adjMat
    attrLines <- return $ length attrMat
    adjNum <- return $ getLength (head unrefined_graph)
    attrNum <- return $ getLength (head unrefined_attr)
    debug $ show (adjLines, attrLines, attrNum)

    ----- CHECK FOR ERRORS
    ---- print out any read-errors and abort
    -- parser-errors
    checkError $ B.intercalate (B.singleton '\n') (rights unrefined_graph)
    checkError $ B.intercalate (B.singleton '\n') (rights unrefined_attr)
    checkError $ either (\a -> B.empty) (\b -> b) $ paramsParsed
    
    paramsFinal <- return $!! case paramsParsed of Left a -> a
    attrParams <- return $ length (range paramsFinal)
    
    -- attribute-errors
    if adjLines /= attrLines then
        checkError $ B.pack $ "Adjacency Matrix size "++ show adjLines ++
                              " differs from Attribute Matrix " ++ show attrLines ++
                              ".\n"
    else return ()
    
    if adjLines /= adjNum then
        checkError $ B.pack $ "Adjacency Matrix is not square.\n" ++
                              "Read format is " ++ show adjLines ++
                              "x" ++ show adjNum ++ ".\n"
    else return ()
        
    -- it is accaptable if the parameters file contains more attributes than the attribute matrix
    if attrParams < attrNum then
        checkError $ B.pack $ "Attribute Matrix format does not match Parameter.\n" ++
                              "Attribute Matrix has " ++ show attrNum ++ " attributes.\n" ++
                              "Parameters implicate" ++ show attrParams ++ " attributes.\n"
    else return ()

    ----- EXTRACT MATRICES
    graph <- return $ A.fromListUnboxed (Z :. adjLines :. adjLines) (L.foldl1 (++) (lefts unrefined_graph)) -- concatenated graph

    attr <- return $ A.fromListUnboxed (Z :. attrLines :. attrNum) (L.foldl1 (++) (lefts unrefined_attr)) -- concatenated attr
    timeEndParse <- getCPUTime

    ----- CALCULATE & OUTPUT

    --debug $ "Before: " ++ show (sumAllS graph)
    --timeStartCalc <- getCPUTime -- (total) CPU time is not what we need
    calculation <- return $!! doCalculation graph attr paramsFinal
    --timeEndCalc <- getCPUTime
    B.putStr calculation
    --putStrLn ("read/parse CPU time: " ++ show (fromIntegral (timeEndParse - timeStartProg) / 1000000000) ++ "ms")
    --putStrLn ("calculation CPU time: " ++ show (fromIntegral (timeEndCalc - timeStartCalc) / 1000000000) ++ "ms")
    
{---TIMINGS

SINGLE CORE
===========
 ./hgraph +RTS -s >result
 197,751,229,488 bytes allocated in the heap
     290,034,880 bytes copied during GC
      11,061,600 bytes maximum residency (10 sample(s))
       1,513,488 bytes maximum slop
              33 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     381500 colls,     0 par    2.24s    2.20s     0.0000s    0.0005s
  Gen  1        10 colls,     0 par    0.05s    0.05s     0.0054s    0.0154s

  TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

  SPARKS: 6266 (0 converted, 0 overflowed, 0 dud, 15 GC'd, 6251 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   74.11s  ( 74.11s elapsed)
  GC      time    2.30s  (  2.25s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   76.42s  ( 76.36s elapsed)

  Alloc rate    2,668,183,845 bytes per MUT second

  Productivity  97.0% of total user, 97.1% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

4 CORES:
========
./hgraph +RTS -s -N4 >result
 197,754,645,560 bytes allocated in the heap
     293,083,624 bytes copied during GC
      11,061,264 bytes maximum residency (10 sample(s))
       1,555,576 bytes maximum slop
              34 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     380952 colls, 380952 par   15.25s    3.92s     0.0000s    0.0255s
  Gen  1        10 colls,     9 par    0.22s    0.06s     0.0056s    0.0181s

  Parallel GC work balance: 1.68% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N4)

  SPARKS: 6266 (6228 converted, 0 overflowed, 0 dud, 30 GC'd, 8 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time  105.25s  ( 86.11s elapsed)
  GC      time   15.47s  (  3.98s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time  120.73s  ( 90.09s elapsed)

  Alloc rate    1,878,861,647 bytes per MUT second

  Productivity  87.2% of total user, 116.8% of total elapsed

gc_alloc_block_sync: 661438
whitehole_spin: 0
gen[0].sync: 655
gen[1].sync: 1347


parallel preprocessing (Adj, Seeds)
==================================
./hgraph +RTS -s -N4 >result
Building hgraph-0.0.1...
Preprocessing executable 'hgraph' for hgraph-0.0.1...
[4 of 5] Compiling DCB.DCB          ( src/DCB/DCB.hs, dist/build/hgraph/hgraph-tmp/DCB/DCB.o )
Linking dist/build/hgraph/hgraph ...
 197,755,802,848 bytes allocated in the heap
     289,986,840 bytes copied during GC
      11,071,880 bytes maximum residency (10 sample(s))
       1,566,376 bytes maximum slop
              34 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     380919 colls, 380919 par   15.73s    3.93s     0.0000s    0.0112s
  Gen  1        10 colls,     9 par    0.28s    0.07s     0.0073s    0.0335s

  Parallel GC work balance: 1.69% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N4)

  SPARKS: 7895 (7825 converted, 0 overflowed, 0 dud, 50 GC'd, 20 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   98.47s  ( 81.37s elapsed)
  GC      time   16.01s  (  4.00s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time  114.49s  ( 85.37s elapsed)

  Alloc rate    2,008,240,220 bytes per MUT second

  Productivity  86.0% of total user, 115.3% of total elapsed

gc_alloc_block_sync: 757575
whitehole_spin: 0
gen[0].sync: 592
gen[1].sync: 510

parallel processing (primitive, too many sparks fizzled) - Speedup: 76.36/51.51 = 1.48  
========================================================
./hgraph +RTS -s -N4 >result
Building hgraph-0.0.1...
Preprocessing executable 'hgraph' for hgraph-0.0.1...
[4 of 5] Compiling DCB.DCB          ( src/DCB/DCB.hs, dist/build/hgraph/hgraph-tmp/DCB/DCB.o )
[5 of 5] Compiling Main             ( src/Main.hs, dist/build/hgraph/hgraph-tmp/Main.o )
Linking dist/build/hgraph/hgraph ...
 205,324,862,344 bytes allocated in the heap
     224,224,264 bytes copied during GC
      11,157,008 bytes maximum residency (9 sample(s))
       1,559,568 bytes maximum slop
              35 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     123063 colls, 123063 par    6.77s    1.67s     0.0000s    0.0074s
  Gen  1         9 colls,     8 par    0.21s    0.06s     0.0061s    0.0190s

  Parallel GC work balance: 8.15% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N4)

  SPARKS: 1714681 (861196 converted, 0 overflowed, 0 dud, 78 GC'd, 853407 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time  145.46s  ( 49.78s elapsed)
  GC      time    6.99s  (  1.73s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time  152.45s  ( 51.51s elapsed)

  Alloc rate    1,411,565,587 bytes per MUT second

  Productivity  95.4% of total user, 282.4% of total elapsed

gc_alloc_block_sync: 378641
whitehole_spin: 0
gen[0].sync: 572
gen[1].sync: 609


parallel processing (monad-par, repa-stuff seqential) - Speedup: 76.36/34,05 = 2.243
=====================================================

./hgraph +RTS -N4 -s > result.txt
 204,368,634,080 bytes allocated in the heap
     306,058,720 bytes copied during GC
      11,108,872 bytes maximum residency (10 sample(s))
       1,597,088 bytes maximum slop
              35 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     108838 colls, 108838 par    9.21s    2.29s     0.0000s    0.0020s
  Gen  1        10 colls,     9 par    0.32s    0.08s     0.0083s    0.0245s

  Parallel GC work balance: 29.41% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N4)

  SPARKS: 15737 (14412 converted, 0 overflowed, 0 dud, 1251 GC'd, 74 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time  124.37s  ( 31.67s elapsed)
  GC      time    9.53s  (  2.37s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time  133.91s  ( 34.05s elapsed)

  Alloc rate    1,643,242,747 bytes per MUT second

  Productivity  92.9% of total user, 365.3% of total elapsed

gc_alloc_block_sync: 531144
whitehole_spin: 0
gen[0].sync: 758
gen[1].sync: 17
        

ADDITIONAL OVERHEAD (running on 1 Core with parallel stuff): Slowdown: 76.36/123.39 = 0.62885
============================================================

./hgraph +RTS -N1 -s > result.txt
 204,364,490,096 bytes allocated in the heap
     291,824,120 bytes copied during GC
      10,081,664 bytes maximum residency (11 sample(s))
       1,545,536 bytes maximum slop
              30 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     393965 colls,     0 par    6.77s    6.72s     0.0000s    0.0017s
  Gen  1        11 colls,     0 par    0.09s    0.09s     0.0079s    0.0217s

  TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

  SPARKS: 15737 (0 converted, 0 overflowed, 0 dud, 166 GC'd, 15571 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time  116.53s  (116.52s elapsed)
  GC      time    6.85s  (  6.80s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time  123.39s  (123.32s elapsed)

  Alloc rate    1,753,707,727 bytes per MUT second

  Productivity  94.4% of total user, 94.5% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0


REAL SPEEDUP AGAINST OVERHEAD-VARIANT: 129.39/34.05 = 3.8
=========================================================

-}
