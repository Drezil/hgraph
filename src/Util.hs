-- | A collection of utility functions for working with functions, lists and strings.
module Util where

import           Control.Parallel.Strategies
import qualified Data.Map as Map

-- | Move first argument to first place (for style uniformity)
flip1 :: (a -> b) -> (a -> b)
flip1 = id

-- | Move second argument to first place ('flip' synonym for style uniformity)
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 = flip

-- | Move third argument to first place
flip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip3 fun c a b = fun a b c

-- | Move fourth argument to first place
flip4 :: (a -> b -> c -> d -> e) -> d -> a -> b -> c -> e
flip4 fun d a b c = fun a b c d 

-- | Move fifth argument to first place
flip5 :: (a -> b -> c -> d -> e -> f) -> e -> a -> b -> c -> d -> f
flip5 fun e a b c d = fun a b c d e 

-- | Move sixth argument to first place
flip6 :: (a -> b -> c -> d -> e -> f -> g) -> f -> a -> b -> c -> d -> e -> g 
flip6 fun f a b c d e = fun a b c d e f

-- | Move seventh argument to first place
flip7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> g -> a -> b -> c -> d -> e -> f -> h
flip7 fun g a b c d e f = fun a b c d e f g

-- | Move eighths argument to first place
flip8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> h -> a -> b -> c -> d -> e -> f -> g -> i
flip8 fun h a b c d e f g = fun a b c d e f g h

-- | Move ninths argument to first place
flip9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> i -> a -> b -> c -> d -> e -> f -> g -> h -> j
flip9 fun i a b c d e f g h = fun a b c d e f g h i


-- | Move first argument to last place (for style uniformity)
flipto1 :: (a -> b) -> (a -> b)
flipto1 = id

-- | Move first argument to last (second) place ('flip' synonym for style uniformity)
flipto2 :: (a -> b -> c) -> (b -> a -> c)
flipto2 = flip

-- | Move first argument to last (third) place
flipto3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flipto3 fun b c a = fun a b c 

-- | Move first argument to last (forth) place
flipto4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
flipto4 fun b c d a = fun a b c d 

-- | Move first argument to last (fifth) place
flipto5 :: (a -> b -> c -> d -> e -> f) -> b -> c -> d -> e -> a -> f
flipto5 fun b c d e a = fun a b c d e 

-- | Move first argument to last (sixth) place
flipto6 :: (a -> b -> c -> d -> e -> f -> g) -> b -> c -> d -> e -> f-> a -> g
flipto6 fun b c d e f a = fun a b c d e f

-- | Move first argument to last (seventh) place
flipto7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> b -> c -> d -> e -> f -> g -> a -> h
flipto7 fun b c d e f g a = fun a b c d e f g

-- | Move first argument to last (eights) place
flipto8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> b -> c -> d -> e -> f -> g -> h -> a -> i
flipto8 fun b c d e f g h a = fun a b c d e f g h

-- | Move first argument to last (ninth) place
flipto9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> b -> c -> d -> e -> f -> g -> h -> i -> a -> j
flipto9 fun b c d e f g h i a = fun a b c d e f g h i

infixl 1 +||

-- | short for a `using` b. We don't need brackets this way and are able to comment out parallelism.
(+||) :: a -> Strategy a -> a
a +|| b = a `using` b

-- | The function 'appendS' appends the string representation of the third element
--   to the second element followed by the first element as separator string.
appendS :: (Show a) => String -> String -> a -> String
appendS sep a b = (a ++ show b) ++ sep

-- I thought I needed those function... When I realised my mistake I
-- did not want to remove them again ;-(
-- | Removes repetitions from a list. An element is only considered a
--   duplication if it equals the previous element. Special case of
--   'remDupsBy' with fixed '==' function.
remDups :: (Eq a) => [a] -> [a]
remDups l = remDupsBy (==) l 

-- | Removes repetitions from a list. An element is only considered a
--   duplication if it equals the previous element.
remDupsBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
remDupsBy f [] = []
remDupsBy f (l:ls) = l:(remDups' l ls)
    where remDups' l [] = []
          remDups' prev (l:ls) = case f prev l of
                                        True  -> remDups' prev ls
                                        False -> l:(remDups' l ls) 

-- When removing duplicates, the first function assigns the input to a bucket,
-- the second function checks whether it is already in the bucket (linear search).
-- | /O(n^2)./ Removes duplicate elements from a list. Performs better than
--   'Data.List.nub' by exploiting features of the 'Ord' class.
ordNubBy :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> [a] -> [a]
ordNubBy p f l = go Map.empty l
  where
    go _ []     = []
    go m (x:xs) = let b = p x in case b `Map.lookup` m of
                    Nothing     -> x : go (Map.insert b [x] m) xs
                    Just bucket
                      | elem_by f x bucket -> go m xs
                      | otherwise          -> x : go (Map.insert b (x:bucket) m) xs

    -- From the Data.List source code.
    elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
    elem_by _  _ []     = False
    elem_by eq y (x:xs) = y `eq` x || elem_by eq y xs


-- | Returns whether a string only contains whitespace characters or not.
isWhitespace :: String -> Bool
isWhitespace "" = True
isWhitespace (a:as) = (a `elem` " \r\n\t") && isWhitespace as

-- | Tests whether an 'Either' type is 'Left'.
isLeft :: Either a b -> Bool
isLeft a = either (\e -> True) (\e -> False) a

-- | Tests whether an 'Either' type is 'Right'.    
isRight :: Either a b -> Bool
isRight = not . isLeft

-- | The 'part' function takes a predicate and a list of tuples and returns
--   the pair of lists of left elements which do and right elements which do not satisfy the
--   predicate, respectively; i. e.,
--
-- > part (\a b -> elem a b) [(1, [1, 3]), (2, [3, 4]), (0, [0, 5])] == ([1, 0], [[3, 4]])
part :: (a -> b -> Bool) -> [(a, b)] -> ([a], [b])
part p xs = foldr (select p) ([], []) xs
    where
        select :: (a -> b -> Bool) -> (a, b) -> ([a], [b]) -> ([a], [b])
        select p (t,f) ~(ts,fs) | p t f     = (t:ts,fs)
                                | otherwise = (ts, f:fs)