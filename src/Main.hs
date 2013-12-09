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
--import           Debug.Trace
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
                omega = (A.fromListUnboxed (ix1 6) [0,5,3,300,5,10])
                delta = 2
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
debug a = return () --putStrLn a


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

    --debug $ "Before: " ++ show (sumAllS graph)
    B.putStr $ doCalculation graph attr

    
    
    
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