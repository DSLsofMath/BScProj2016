> {-# LANGUAGE TypeSynonymInstances #-}
> -- | Module for polynomials, used in Fourier series and Fourier Transform

> module Polynomials where

> import ComplexNumbers
> data Poly a  =  Single a  |  Cons a (Poly a)
>                 deriving (Eq, Ord)

The relationship between `Poly a` and `[a]` is given by the following
functions:

> toList :: Poly a -> [a]
> toList (Single a)   =  [a]
> toList (Cons a as)  =  a : toList as

> fromList :: [a] -> Poly a
> fromList [a] = Single a
> fromList (a0 : a1 : as) = Cons a0 (fromList (a1 : as))

> instance Show a => Show (Poly a) where
>   show = show . toList

Since we only use the arithmetical operations, we can generalise our
evaluator:

> evalPoly ::  Num a => Poly a -> a -> a
> evalPoly (Single a)     x   =  a
> evalPoly (Cons a as)    x   =  a + x * evalPoly as x

> instance Num a => Num (Poly a) where
>   Single a   +  Single b   =  Single (a + b)
>   Single a   +  Cons b bs  =  Cons (a + b) bs
>   Cons a as  +  Single b   =  Cons (a + b) as
>   Cons a as  +  Cons b bs  =  Cons (a + b) (as + bs)
>
>   Single a   *  Single b   =  Single (a * b)
>   Single a   *  Cons b bs  =  Cons (a * b) (Single a * bs)
>   Cons a as  *  Single b   =  Cons (a * b) (as * Single b)
>   Cons a as  *  Cons b bs  =  Cons (a * b) (as * Cons b bs + Single a * bs)
>
>   negate (Single a)        =  Single (negate a)
>   negate (Cons a as)       =  Cons (negate a) (negate as)
>
>   fromInteger              =  Single . fromInteger
> 
>   abs (Single a)           = Single (abs a)
>   abs (Cons a as)          = Cons (abs a) (abs as)
>   signum p                 = undefined

>
> type PowerSeries a = Poly a

> eval n as x = evalPoly (takePoly n as) x

> takePoly :: Integer -> Poly a -> Poly a
> takePoly n (Single a)   =  Single a
> takePoly n (Cons a as)  =  if n <= 1
>                            then  Single a
>                            else  Cons a (takePoly (n-1) as)

> instance (Eq a, Fractional a) => Fractional (PowerSeries a) where
>   as / Single b           =  as * Single (1 / b)
>   Single a / Cons b bs    =  if a == 0 then Single 0 else Cons a (Single 0) / Cons b bs
>   Cons a as / Cons b bs   =  let  q = a / b
>                              in   Cons q  ((as - Single q * bs) / Cons b bs)
>   fromRational            =  Single . fromRational

> test0, test1, test2 :: PowerSeries Complex
> test0 = 1 / (1 - x)
> test1 = 1 / (1 - x)^2
> test2 = (x^2 - 2 * x + 1) / (x - 1)

> x :: Num a => Poly a
> x = Cons 0 (Single 1)

> deriv (Single a)   =  Single 0
> deriv (Cons a as)  =  deriv' as 1
>                       where deriv' (Single a)  n  =  Single (n * a)
>                             deriv' (Cons a as) n  =  Cons (n * a) (deriv' as (n+1))
