
> module ComplexNumbers.Test where
> import ComplexNumbers
> import ComplexNumbers.Parse
> import Test.QuickCheck

> mapFst :: (a->b) -> (a, c) -> (b, c)
> mapFst f (a, c) = (f a, c)
> mapReadS :: (a->b) -> (ReadS a -> ReadS b)
> mapReadS f p = map (mapFst f) . p

> instance Read Complex where
>   readsPrec p = mapReadS (toComplex . unPair) (readsPrec p)
>     where toComplex (re,im) = Complex re im

> f =^= g = \x -> f x == g x
> testRoundTrip = (read . printComplex) =^= id

  Arbitrary instans för komplexa tal, tillåter QuickCheck att generera
  godtyckliga komplexa tal, där realdelen och imaginärdelen är
  godtyckliga doubles.

> instance Arbitrary Complex where
>    arbitrary = do
>       re <- arbitrary
>       im <- arbitrary
>       return (Complex re im)
