module Util where

import           Control.Parallel.Strategies

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

appendS :: (Show a) => String -> String -> a -> String
appendS sep a b = (a ++ show b) ++ sep

