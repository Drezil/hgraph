module Util where

-- | Move first argument to first place (for style uniformity)
flip1 :: (a -> b) -> (a -> b)
flip1 = id

-- | Move second argument to first place ('flip' synonym for style uniformity)
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 = flip

-- | Move third argument to first place
flip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip3 f c a b = f a b c

-- | Move fourth argument to first place
flip4 :: (a -> b -> c -> d -> e) -> d -> a -> b -> c -> e
flip4 f d a b c = f a b c d 


-- | Move first argument to last place (for style uniformity)
flipto1 :: (a -> b) -> (a -> b)
flipto1 = id

-- | Move second argument to last place ('flip' synonym for style uniformity)
flipto2 :: (a -> b -> c) -> (b -> a -> c)
flipto2 = flip

-- | Move third argument to last place
flipto3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flipto3 fun b c a = fun a b c 

-- | Move forth argument to last place
flipto4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
flipto4 fun b c d a = fun a b c d 

-- | Move fifth argument to last place
flipto5 :: (a -> b -> c -> d -> e -> f) -> b -> c -> d -> e -> a -> f
flipto5 fun b c d e a = fun a b c d e 

-- | Move sixth argument to last place
flipto6 :: (a -> b -> c -> d -> e -> f -> g) -> b -> c -> d -> e -> f-> a -> g
flipto6 fun b c d e f a = fun a b c d e f

-- | Move seventh argument to last place
flipto7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> b -> c -> d -> e -> f -> g -> a -> h
flipto7 fun b c d e f g a = fun a b c d e f g

-- | Move eights argument to last place
flipto8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> b -> c -> d -> e -> f -> g -> h -> a -> i
flipto8 fun b c d e f g h a = fun a b c d e f g h

-- | Move ninth argument to last place
flipto9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> b -> c -> d -> e -> f -> g -> h -> i -> a -> j
flipto9 fun b c d e f g h i a = fun a b c d e f g h i