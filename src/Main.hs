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
-- Program to find densely connected biclusters (DCB) in an undirected graph.
-- DCB are highly connected subgraphs whose nodes share similar attributes.
-- 
-- Each node of the source graph is linked with a table of attribute values
-- and for a certain number of attributes all nodes of a DCB must have
-- values within a specified range. Secondly, the density of the
-- subgraph must not exceed a threshold.
--
-- /Using the program:/
-- 
-- >>> hgraph [<tokens>]... <adjacency> <attribute> <constraints>
--
-- The program expects 3 mandatory arguments for an input graph of size /n/:
-- 
--  (1) The plain text file that contains the adjacency matrix of the source
--      graph. The file must contain /n/ rows where each row consists of
--      exactly /n/ characters that are either @1@ if there exists an edge
--      between two nodes or @0@ otherwise.
--      The matrix must be symmetric.
--  
--  (2) The plain text file that contains the attributes linked to the nodes.
--      It must consist of /n/ rows.
--      Each row consists of /a/ floating point numbers separated by tab
--      characters @\\t@ where /a/ is the number of attributes.
--      The /b/-th value in the /v/-th row is the attribute value of the /b/-th
--      attribute linked to the /v/-th node.
--  
--  (3) The plain text file that contains the algorithm’s parameters. This file
--      contains 3 rows:
-- 
--       * The first row solely contains the minimum density for a DCB as floating
--         point number
-- 
--       * The second row solely contains the minimum number of matching
--         attributes for a DCB as an integer value.
-- 
--       * The third row contains the tolerances for each attribute as floating
--         point values separated by tab characters @\\t@ where the /b/-th value
--         is the tolerated range for the DCB’s node’s values of the /b/-th
--         attribute.
-- 
-- Rows are separated by newline characters @\\n@ and all floating point values
-- use dots as decimal separator.
-- 
-- You may prepend the mandatory parameters with certain tokens:
--
-- * @-h@ or @--help@ to display help (all following parameters are ignored)
-- 
-- * @-t@ or @--time@ to measure and display computation time
-- 
-- To enable multicore computation you need to append RTS options to the
-- program call as specified in the GHC manual.
-- 
-- <http://www.haskell.org/ghc/docs/latest/html/users_guide/using-smp.html#parallel-options>
--
-- Running on 4 threads:
-- 
-- >>> hgraph adjacency_matrix.adj attribute_matrix.attr properties.p +RTS -N4
--
----------------------------------------------------------------------------

module Main where

import           DCB.DCB
import           DCB.Structures
import           DCB.IO
import           Util

import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad                  (when)
--import           Control.Monad.Par.Scheds.Trace
import           Control.Parallel.Strategies
import qualified Data.Array.Repa                as A hiding ((++))
import           Data.Array.Repa.Index
import           Data.Array.Repa.Eval           (Elt)
import           Data.Array.Repa.Repr.Unboxed
import           Data.Array.Repa.Repr.Vector
import           Data.ByteString.Char8          (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.ByteString.Lex.Double     as B
import           Data.Char                      (isSpace)
import           Data.Either                    (lefts, rights)
import           Data.Functor.Identity
import           Data.Int
import qualified Data.List                      as L
import           Data.Time.Clock
import qualified Data.Vector.Unboxed            as V
--import           Debug.Trace
import           System.CPUTime
import           System.Environment
import           System.Exit                    (exitFailure, exitSuccess)
--import           Test.QuickCheck.All            (quickCheckAll)

-- | All secondary parameters that must be passed to the program packed into one data type.
data Params = Params { density :: Double -- ^ minimum density of a bicluster
                     , matches :: Int -- ^ required amount of matching attributes
                     , range   :: [Double] -- ^ allowed divergence for each attribute
                     } deriving (Show,Eq)

instance NFData Params


-- | Parses a row of the adjacency matrix of a graph. The consistency of line lengths is not tested 
--   by this function! In case of a successfull parse a 'Left' ('Vector' a) is returned, otherwise a
--   'Right' 'ByteString' containing an error message.
--   
--   > 10101
--   > 01010
--   > 00100
--   > 01010
--   > 10101
--   
--    * Valid Values: @0@, @1@
--   
--    * any invalid value raises an error
parseAdjMat :: (Num a, Unbox a) => ByteString -> Either (V.Vector a) ByteString
parseAdjMat input =
    let
        size = B.length input
        result = V.unfoldrN size parseAdjMat' input
    in
        if size == V.length result then Left result
                            else Right $ B.append (B.pack "(adjacency)cannot parse ") input
  where
    --parseAdjMat' :: ByteString -> Maybe (a, ByteString)
    parseAdjMat' input =
        let c = B.head input in
        case c of
             '0' -> Just (0, B.tail input)
             '1' -> Just (1, B.tail input)
             _   -> Nothing

-- | Tests if a parse result @(a, 'ByteString')@ is considered valid where @a@ is the parsed object
--   and the 'ByteString' is the remaining string that could not be parsed.
testParse :: Maybe (a, ByteString) -> Maybe a
testParse Nothing = Nothing
testParse (Just (a, rem)) = if emptyLine rem then Just a else Nothing

-- | Parses a row of the attribute matrix of a graph. The consistency of line lengths is not tested 
--   by this function! In case of a successfull parse a @'Left' [a]@ is returned, otherwise a
--   @'Right' 'ByteString'@ containing the error message.
-- 
--   > 1     2.3
--   > -1    2.1
--   > 4     2.7
--   > 2.2   -3e-4
--   > 3     2.3
--
--   * Valid: 'Double's divided by specified delimter
--   
--   * any invalid value raises an error
parseAttr :: Char -- ^ delimiter
          -> ByteString -- ^ text to parse
          -> Either [Double] ByteString
parseAttr delim input = parseAttr' (B.split delim input)
    where parseAttr' :: [ByteString] -> Either [Double] ByteString
          parseAttr' (row:rem) =
              case testParse $ B.readDouble row of
                   Just d -> case parseAttr' rem of
                                  Left l -> Left $! d:l
                                  r      -> r
                   _      -> Right $ B.append (B.pack "(attr)cannot parse ") row
          parseAttr' [] = Left []

-- | Parses parameter file.
--  
--    * First line: 'Density'
--   
--    * Second line: requied matches ('Int')
--   
--    * Third line is the tolerance for each attribute ('Double' values), see 'parseAttr'
--   
--   In case of an error during the parsing a @'Right' 'ByteString'@ containing the error
--   message is returned instead of a @'Left' 'Params'@
parseParams :: Char -- ^delimiter
            -> [ByteString] -- ^ text to parse
            -> Either Params ByteString
parseParams delim input
  | length input /= 3 = Right $ B.pack ("(params)amount of lines does not match (expected 3, got "
                                ++ show (length input) ++ ")")
  | otherwise =
    case testParse $ B.readDouble (head input) of -- parse density
         Just dens -> case testParse $ B.readInt (head $ tail input) of -- parse matches
                           Just match
                               -> case parseAttr delim (head $ tail $ tail input) of --parse range line
                                       Left range -> Left $ Params dens match range
                                       Right msg  -> Right $ B.append (B.pack "(param - range)") msg
                           _   -> Right $ B.append (B.pack "(param - match)cannot parse ") (head $ tail input)
         _         -> Right $ B.append (B.pack "(param - density)cannot parse ") (head input)


-- | Checks if a given text is empty (empty string, whitespaces).
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
                        doAll gs a b c d e = maxDCB (step gs a b c d e) a b c d e
                        
                {-- commented out: all solutions, not only maximum DCB
                        -- don't print out seeds
                        doAll [] _ _ _ _ _ = []
                        doAll gs a b c d e = doAll' (step gs a b c d e) a b c d e
                        -- but everything in the following recursive calls
                        doAll' [] _ _ _ _ _ = []
                        doAll' gs a b c d e = gs ++ doAll' (step gs a b c d e) a b c d e
                --}

-- | Gets the length of the @'Left' [a]@.
--
--   @0@ if @[a]@ in @Left [a]@ is empty or it is a 'Right' value.
getLength :: Either [a] b -> Int
getLength (Left a) = length a
getLength (Right _) = 0

-- | Gets the length of the @'Left' ('Vector' v)@.
--
--   @0@ if @a@ in 'Left a' is empty or it is a 'Right' value.
getLengthV :: (Unbox a) => Either (V.Vector a) b -> Int
getLengthV (Left a) = V.length a
getLengthV (Right _) = 0

-- | Prints the help and exits.
showHelp :: IO ()
showHelp = do
                putStrLn $ "Usage: hgraph [<tokens>]... <adjacency> <attribute> <constraints>\n"++
                           "\nTokens:\n"++
                           "-h, --help   show help\n"++
                           "-t, --time   show timings at the end of output\n"++
                           "\n"++
                           "adjacency    An adjecency-matrix with 0 or 1 as weights for edges\n"++
                           "             seperated by newlines for each row.\n"++
                           "             Must be NxN.\n"++
                           "\n"++
                           "attribute    A tabulator-seperated Matrix of attributes.\n"++
                           "             The attribute lists for each node are seperated by newlines.\n"++
                           "             Must be Nxk (k amount of attributes).\n"++
                           "\n" ++
                           "constraints  A properties file containing the algorithms constraints.\n"++
                           "             First line:  required DCB density (dot-seperated decimal value).\n"++
                           "             Second line: minimum amount of matching attributes (integer value).\n"++
                           "             Third line:  tabulator-seperated list of attribute thresholds,\n"++
                           "                          decimal values, must contain k entries.\n"++
                           "\n"
                exitSuccess

-- | Checks if the submitted text is empty. If not it will be printed out and the program aborts.
checkError :: ByteString -> IO ()
checkError a
        | emptyLine a  = return ()
        | otherwise = do
                        B.putStr $ B.append (B.append (B.pack "Error detected:\n") a) (B.pack "\n\n")
                        exitFailure

-- | convinience debug-function. Needs to be
--   changed to return () to disable Debug.
debug a = return () --putStrLn a

-- | Removes one trailing carriage return character @\\r@ if existent.
removeCarriageReturn :: ByteString -> ByteString
removeCarriageReturn input =
    if B.last input == '\r' then B.init input
                            else input

-- | Processes the tokens of input parameters.
--   
--   Valid tokens are /show time/ (@-t@, @--time@) and /display help/
--   (@-h@, @--help@) whereby one-character tokens may be combined into
--   a single list (@-th@).
--   
--   If any parameter is no token (including token lists that contain
--   invalid tokens) that parameter and all following parameters are
--   believed to be file paths.
--   
--   The returned tupel contains the existance of the /time/ token,
--   existance of the /help/ token and all remaining file paths in that
--   order.
--
--   >>> processTokens ["--time", "-t", "foo.txt"] == (True, False, ["foo.txt"])
--   >>> processTokens ["-tx", "--help"] == (False, False, ["-tx", "--help"])
processTokens :: [String] -> (Bool, Bool, [FilePath])
processTokens [] = (False, False, [])
-- single token with prefix "--"
processTokens x@(('-':'-':t):xs) = 
    let (nT, nH, rem) = processTokens xs
    in case t of
            "help" -> (nT, True, rem)
            "time" -> (True, nH, rem)
            _ -> (False, False, x)
-- list of token abbreviations with prefix "-" (e. g. "-t", "-th")
processTokens x@(('-':t@(_:[])):xs) =
    let --                    (valid,time,  help )
        processTokenList "" = (True, False, False)
        processTokenList (t:ts) = if not isValid then (False, False, False) else
            case t of
                't' -> (True, True, nH)
                'h' -> (True, nT, True)
                _   -> (False, False, False)
            where (isValid, nT, nH) = processTokenList ts
        (v, nT, nH) = processTokenList t  
        (nT', nH', rem) = processTokens xs
    in if v
          then (nT || nT', nH || nH', rem)
          else (False, False, x)
processTokens x = (False, False, x)

-- | The main-function to bootstrap our application
main = do
    timeStartProg <- getCurrentTime
    args <- getArgs
    (dispTime, wantHelp, paths) <- return $ processTokens args
    when wantHelp showHelp  -- terminates
    input <- if length paths == 3 then Prelude.mapM B.readFile paths
             else error "Error: Wrong number of files given. Try --help for more information."

    
    -- read file and clean
    adjMat <- return $ L.map removeCarriageReturn $ L.filter (not . emptyLine) (B.lines (head input))
    attrMat <- return $ L.map removeCarriageReturn $ L.filter (not . emptyLine) (B.lines (head $ tail input))
    paramRef <- return $ L.map removeCarriageReturn $ L.filter (not . emptyLine) (B.lines (head $ tail $ tail input))
   

    unrefined_graph <- return $ (L.map (parseAdjMat) adjMat)
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
    unrefined_attr <- return $ (L.map (parseAttr '\t') attrMat)
                                        -- +|| (parBuffer 25 rseq) --run parallel, evaluate fully
    paramsParsed <- return $ parseParams '\t' paramRef
    
    adjLines <- return $ length adjMat
    attrLines <- return $ length attrMat
    adjNum <- return $ getLengthV (head unrefined_graph)
    attrNum <- return $ getLength (head unrefined_attr)
    debug $ show (adjLines, attrLines, attrNum)

    ----- CHECK FOR ERRORS
    ---- print out any read-errors and abort
    -- parser-errors
    checkError $ B.intercalate (B.singleton '\n') (rights unrefined_graph)
    checkError $ B.intercalate (B.singleton '\n') (rights unrefined_attr)
    checkError $ either (\a -> B.empty) (\b -> b) $ paramsParsed
    
    paramsFinal <- return $ case paramsParsed of Left a -> a
    attrParams <- return $ length (range paramsFinal)
    
    -- attribute-errors
    if adjLines /= attrLines then
        checkError $ B.pack $ "Adjacency Matrix size "++ show adjLines ++
                              " differs from Attribute Matrix " ++ show attrLines ++
                              "."
    else return ()
    
    if adjLines /= adjNum then
        checkError $ B.pack $ "Adjacency Matrix is not square.\n" ++
                              "Read format is " ++ show adjLines ++
                              "x" ++ show adjNum ++ "."
    else return ()
        
    -- it is accaptable if the parameters file contains more attributes than the attribute matrix
    if attrParams < attrNum then
        checkError $ B.pack $ "Attribute Matrix format does not match Parameter.\n" ++
                              "Attribute Matrix has " ++ show attrNum ++ " attributes.\n" ++
                              "Parameters implicate " ++ show attrParams ++ " attributes."
    else return ()

    ----- EXTRACT MATRICES
    graph <- return $ A.fromUnboxed (Z :. adjLines :. adjLines) (V.concat (lefts unrefined_graph)) -- concatenated graph

    attr <- return $ A.fromListUnboxed (Z :. attrLines :. attrNum) (L.foldl1 (++) (lefts unrefined_attr)) -- concatenated attr
    
    --t1 <- getCurrentTime
    evaluate graph
    --t2 <- getCurrentTime
    --putStrLn ("graph: " ++ show (diffUTCTime t2 t1))
    evaluate attr
    --t3 <- getCurrentTime
    --putStrLn ("attr: " ++ show (diffUTCTime t3 t2))
    evaluate paramsFinal
    --t4 <- getCurrentTime
    --putStrLn ("param: " ++ show (diffUTCTime t4 t3))
    timeEndParse <- getCurrentTime

    ----- CALCULATE & OUTPUT

    --debug $ "Before: " ++ show (sumAllS graph)
    timeStartCalc <- getCurrentTime
    calculation <- return $!! doCalculation graph attr paramsFinal
    timeEndCalc <- getCurrentTime
    B.putStr calculation
    
    when dispTime $
        putStrLn ("read/parse CPU time: " ++ show (diffUTCTime timeEndParse timeStartProg)
            ++ "\ncalculation CPU time: " ++ show (diffUTCTime  timeEndCalc timeStartCalc))
    
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
