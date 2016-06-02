> module ComplexNumbers.Parse where
> import Control.Applicative (Applicative(..))
> import Control.Monad       (liftM, ap)
> import ComplexNumbers

Eftersom projektet fokuserar på matematik är parsning inte
centralt. Därför lägger jag detta i en separat fil.

Steg 1: använd en typ för parsning.

Parsning är helt område för sig, men för enkla situationer som Complex
räcker det med standardtypen i Haskell:

type ReadS a = String -> [(a, String)]

som vi kan bädda in och göra till en Monad.

> newtype P a = P {unP :: ReadS a}

> instance Monad P where
>   return = returnP
>   (>>=)  = bindP
>   -- fail   = failP

> returnP :: a -> P a
> returnP a = P $ \s->[(a,s)]

> bindP :: P a -> (a -> P b) -> P b
> bindP (P p) g = P $ concatMap (uncurry (unP . g)) . p

Vi behöver också en implementation av "eller":

> orP :: P a -> P a -> P a
> orP (P p) (P q) = P (\s->p s ++ q s)
> (|||) = orP

(Det finns effektivare lösningar, men det här duger för enkla exempel.)

samt ett sätt att kräva en viss symbol (ett visst tecken):

> symbolP :: Char -> P Char
> symbolP c = P sP
>   where sP [] = []
>         sP (x:cs) | c == x    =  [(c, cs)]
>                   | otherwise =  []

Steg 2: Definiera en grammatik

C ::=  R [ ('+' | '-') R 'j' ]

Ge några exempel som ingår i språket definierat av den här grammatiken:

> ex1 = ["3", "1+2j", "2-4j"]
> ex1' :: Num a => [Pair a]
> ex1' = map Pair [(3,0), (1,2), (2,-4)]
> check1D = map read ex1 == (ex1' :: [Pair Double])
> check1I = map read ex1 == (ex1' :: [Pair Int])

och några motexempel (som man kanske skulle vilja hantera).

> notEx1 = ["2j", "+1", "1+2i", "1 + 2j"]

Steg 3: Översätt grammatiken till en parser

> pairP :: (Num a, Read a) => P (Pair a)
> pairP = do re <- realP
>            im <- return 0 ||| signedImP
>            return $ Pair (re, im)

> complexP :: P Complex
> complexP = do re <- realP
>               im <- return 0 ||| signedImP
>               return $ Complex re im

> signedImP :: (Num a, Read a) => P a
> signedImP = do si <- signP
>                im <- realP
>                _  <- symbolP 'j'
>                return $ si im

> signP :: Num a => P (a->a)
> signP = (symbolP '+' >> return id) ||| (symbolP '-' >> return negate)

Notera att denna lösning kommer att tillåta för mycket (exempelvis
1+-1j) men också för lite (godkänner inte tomrum).

> realP :: Read a => P a
> realP = P reads

Steg 4: Skriv en Read-instans

> newtype Pair a = Pair {unPair :: (a, a)}
>   deriving (Eq, Show)

> instance (Read a, Num a) => Read (Pair a) where
>   readsPrec _p = unP pairP

> instance Read Complex where
>   readsPrec _p = unP complexP

Ett par hjälpinstanser.

> instance Functor P where
>   fmap = liftM

> instance Applicative P where
>   pure = returnP
>   (<*>) = ap
