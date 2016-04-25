
> module ComplexNumbers.Test where
> import ComplexNumbers
> import ComplexNumbers.Parse
> import Test.QuickCheck

> mapFst :: (a->b) -> (a, c) -> (b, c)
> mapFst f (a, c) = (f a, c)
> mapReadS :: (a->b) -> (ReadS a -> ReadS b)
> mapReadS f p = map (mapFst f) . p
>
> instance Read Complex where
>   readsPrec p = mapReadS (Complex . unPair) (readsPrec p)
>
> f =^= g = \x -> f x == g x
> testRoundTrip  =  (read . printComplex) =^= id
>
>
