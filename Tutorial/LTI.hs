{-# LANGUAGE FlexibleInstances #-}

import qualified Test.QuickCheck as Q

--Tiden kan vara kontinuelig eller diskret, double eller int.
type ContTime = Double
type DiscTime = Integer

type Signal a b = (a -> b)

type ContSignal a = Signal ContTime a
type DiscSignal a = Signal DiscTime a

--Gäller för alla funktioner double -> double, t.ex. sin
type ContTimeFun = ContSignal Double
type DiscTimeFun = DiscSignal Double

--Kontinuerlig Impuls: Oändlig om t=0, annars 0
contImpulse :: ContTimeFun
contImpulse t | t == 0 = 1/0
              | otherwise = 0

--Diskret Impuls: 1 om t=0, annars 0
discImpulse :: DiscTimeFun
discImpulse t | t == 0 = 1
              | otherwise = 0

--Kontinuerligt enhetssteg: 0 om t<0, 1 om t >= 0
contStep :: ContTimeFun
contStep t | t < 0 = 0
--HalfMaximumConvention, ett vanligt sätt skriva för t=0
           | t == 0 = 1/0
           | t > 0 = 1

--Diskret enhetssteg: 0 om t<0, 1 om t >= 0
discStep :: DiscTimeFun
discStep t | t < 0 = 0
           | otherwise = 1

--Definierar vanliga beräkningoperationer, som t.ex. + och *, för Signaler
instance Num b => Num (Signal a b) where
    s0 + s1     = \a -> (s0 a) + (s1 a)
    s0 * s1     = \a -> (s0 a) * (s1 a)
    negate s    = \a -> negate (s a)
    abs s       = \a -> abs (s a)
    signum s    = \a -> signum (s a)
    fromInteger = const . fromInteger

scale :: Num b => Signal a b -> b -> Signal a b
scale sig f = (*f) . sig
infixl 7 `scale`

--En jämförelsefunktion som tillåter väldigt små fel, kan användas för att
--undvika fel som beror på avrundning
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 1.0e-10
infixl 4 ~=

--Approximativ faltning i kontinuerlig tid
contConvolution :: ContTimeFun -- ^ Signal 1
                -> ContTimeFun -- ^ Signal 2
                -> ContTime -- ^ Starttid
                -> ContTime -- ^ Sluttid
                -> Double -- ^ Steg mellan samplingsintervall
                -> ContTimeFun -- ^ Returfunktion
contConvolution s0 s1 start stop step = sum $ map conv points
    where points = [start, step .. stop]
          conv n m = (s0 (n - m)) * (s1 m)

--Faltning i diskret tid
discConvolution :: DiscTimeFun -- ^ Signal 1
                -> DiscTimeFun -- ^ Signal 2
                -> DiscTime -- ^ Interval length -M start
                -> DiscTime -- ^ Interval length M slut
                -> DiscTimeFun -- ^ Returnfunktion
discConvolution s0 s1 start stop = sum $ map conv points
    where points = [start .. stop]
          conv n m = (s0 (n-m)) * (s1 m)

--Ett System kan betraktas som en funktion för signaler
type DiscSystem = DiscTimeFun
type ContSystem = ContTimeFun

timeShift :: Num a => Signal a b -> a -> Signal a b
timeShift sig o = \t -> sig (t - o)

--multSignal :: Int -> Signal -> Signal
--multSignal i s = Signal i s

-- Genererar utsignalen för enkla signaler och system
discOutSignal :: DiscSystem -> DiscTimeFun -> DiscTimeFun
discOutSignal sys insignal = discConvolution insignal sys (-100) 100

contOutSignal :: ContSystem -> ContTimeFun -> ContTimeFun
contOutSignal sys insignal = contConvolution insignal sys (-100) 100 0.1

-- Övning: Implementera ett test för superpositionsprincipen
--Vi har två signaler X och Y.
--Xin(t) -> Xut(t) och Yin(t) -> Yut(t).
--Om systemet uppfyller superpositionsprincipen gäller då att
--a*Xin(t) + b*Yin(t) -> a*Xut(t) + b*Yut(t), där a och b är konstanter
isLinearCont :: ContTimeFun -- ^ Insignal 1
            -> ContTimeFun -- ^ Insignal 2
            -> ContSystem  -- ^ System
            -> Double -- ^ Skalningsfaktor 1
            -> Double -- ^ Skalningsfaktor 2
            -> ContTime -- ^ Tid då vi mäter
            -> Bool
isLinearCont x0 x1 sys a b t = testAt (y0' + y1') (y0 `scale` a + y1 `scale` b) t
    where y0  = contOutSignal sys x0
          y1  = contOutSignal sys x1
          y0' = contOutSignal sys (x0 `scale` a)
          y1' = contOutSignal sys (x1 `scale` b)
          testAt a b t = a t ~= b t

isLinearDisc :: DiscTimeFun -- ^ Insignal 1
            -> DiscTimeFun -- ^ Insignal 2
            -> DiscSystem  -- ^ System
            -> Double -- ^ Skalningsfaktor 1
            -> Double -- ^ Skalningsfaktor 2
            -> DiscTime -- ^ Tid då vi mäter
            -> Bool
isLinearDisc x0 x1 sys a b t = testAt (y0' + y1') (y0 `scale` a + y1 `scale` b) t
    where y0  = discOutSignal sys x0
          y1  = discOutSignal sys x1
          y0' = discOutSignal sys (x0 `scale` a)
          y1' = discOutSignal sys (x1 `scale` b)
          testAt a b t = a t ~= b t

-- | Kollar om ett system är linjärt genom att mata det med två signaler och
-- använder sig av superpositionsprincipen.
prop_isLinearCont :: ContTimeFun -- ^ Insignal 1
                  -> ContTimeFun -- ^ Insignal 2
                  -> ContSystem -- ^ System
                  -> Double -- ^ Skalfaktor 1 (genereras av QuickCheck)
                  -> Double -- ^ Skalfaktor 2 (genereras av QuickCheck)
                  -> ContTime -- ^ Tidspunkt (genereras av QuickCheck)
                  -> Bool
prop_isLinearCont x0 x1 sys = \a b t -> isLinearCont x0 x1 sys a b t

-- | Kollar om ett system är linjärt genom att mata det med två signaler och
-- använder sig av superpositionsprincipen.
prop_isLinearDisc :: DiscTimeFun -- ^ Insignal 1
                  -> DiscTimeFun -- ^ Insignal 2
                  -> DiscSystem -- ^ System
                  -> Double -- ^ Skalfaktor 1 (genereras av QuickCheck)
                  -> Double -- ^ Skalfaktor 2 (genereras av QuickCheck)
                  -> DiscTime -- ^ Tidspunkt (genereras av QuickCheck)
                  -> Bool
prop_isLinearDisc x0 x1 sys = \a b t -> isLinearDisc x0 x1 sys a b t

--Övning: Implementera ett test för tidsinvariansegenskapen, givet timeshift
--Ett system är tidsinvariant om en tidsförskjutning i insignalen ger samma
--tidsförskjutning i utsignalen. Alltså:
--Xin(t-c) -> Xut(t-c), där c är en reell konstant.
isTimeInvCont :: ContTimeFun -> ContSystem -> ContTime -> ContTime -> Bool
isTimeInvCont x sys t c = y' t ~= (timeShift y c) t
    where x' = timeShift x c
          y  = contOutSignal sys x
          y' = contOutSignal sys x'

isTimeInvDisc :: DiscTimeFun -> DiscSystem -> DiscTime -> DiscTime -> Bool
isTimeInvDisc x sys t c = y' t ~= (timeShift y c) t
    where x' = timeShift x c
          y  = discOutSignal sys x
          y' = discOutSignal sys x'

--Ett system är ett LTI-system om det uppfyller superpositionsprincipen och är
--tidsinvariant
isLTICont :: Double -> ContTimeFun -> Double -> ContTimeFun -> ContSystem -> ContTime -> ContTime -> Bool
isLTICont a x b y sys t c = isLinearCont x y sys a b t && isTimeInvCont x sys t c

isLTIDisc :: Double -> DiscTimeFun -> Double -> DiscTimeFun -> DiscSystem -> DiscTime -> DiscTime -> Bool
isLTIDisc a x b y sys t c = isLinearDisc x y sys a b t && isTimeInvDisc x sys t c
