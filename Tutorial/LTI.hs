{-# LANGUAGE FlexibleInstances #-}

module LTI where

import qualified Test.QuickCheck as Q
import qualified Data.List as L

type Signal a b = (a -> b)

class (Ord a, Num a, Enum a, Q.Arbitrary a) => Time a where
  shift :: (a -> b) -> a -> (a -> b)

instance Time Int where
  shift sig offset t = sig (t + offset)

instance Time Integer where
  shift sig offset t = sig (t + offset)

instance Time Double where
  shift sig offset t = sig (t + offset)

--Gäller för alla funktioner double -> double, t.ex. sin

-- Eftersom vi använder oss av en diskret approximation i kontinuerliga fall är
-- enhetsimpulsen 1 vid t=0, annars 0.
-- Notera dock att i helt kontinuerliga uträkningar, med faltningsintegraler och
-- så vidare närmar sig enhetsimpulsens värde vid t=0 oändligheten, men är annars 0.

-- TODO: kommentaren säger att det är en approximation, men då borde
-- det finnas en parameter som anger bredden på impulsen. Klassiskt
-- val är en 0-centrerad normalfördelning med parameter a - se figuren
-- i https://en.wikipedia.org/wiki/Dirac_delta_function
-- https://en.wikipedia.org/wiki/Dirac_delta_function#/media/File:Dirac_function_approximation.gif
-- Det finns andra alternativa former, men gemensamt är att värdet i
-- noll växer när bredden minskar - det blir inte bra att ha ett fixt
-- (ändligt) värde i t=0 och noll i övrigt. En delspecifikation av
-- (contImpulseApprox eps) är att integralen från -Inf till Inf ska
-- vara ett.

-- | Approximativ kontinuerlig enhetsimpuls: 1 om t=0, annars 0
contImpulse eps t | (abs t) < eps = 1 / eps
                  | otherwise = 0

-- | Diskret Impuls: 1 om t=0, annars 0
discImpulse t | t == 0 = 1
              | otherwise = 0

-- Av samma anledning som för contImpulse är vår contSteps värde vid t=0 1,
-- snarare än något som närmar sig oändligheten, vilket är mer riktigt för
-- helt kontinuerliga signaler.

-- TODO: Angående kommentaren ovan: Det är inte korrekt med något
-- värde större än 1 for contStep 0 även om ni vill ha helt
-- kontinuerliga signaler.

-- | Approximativt kontinuerligt enhetssteg: 0 om t<0, 1 om t >= 0
contStep t | t < 0 = 0
           | otherwise = 1

--Diskret enhetssteg: 0 om t<0, 1 om t >= 0
discStep t | t < 0 = 0
           | otherwise = 1

--Definierar vanliga beräkningoperationer, som t.ex. + och *, för Signaler
instance Num val => Num (Signal time val) where
    s0 + s1     = \a -> (s0 a) + (s1 a)
    s0 * s1     = \a -> (s0 a) * (s1 a)
    negate s    = \a -> negate (s a)
    abs s       = \a -> abs (s a)
    signum s    = \a -> signum (s a)
    fromInteger = const . fromInteger

scale sig f = (*f) . sig
infixl 7 `scale`

--En jämförelsefunktion som tillåter väldigt små fel, kan användas för att
--undvika fel som beror på avrundning
-- TODO: be careful with using an "absolute" epsilon for comparison of floating point numbers.
-- Think of a = 1.0e-10 and b = 1.0e-11, then a = 10*b but still a ~= b with this definition!
-- Perhaps use abs (a/b - 1) < eps instead?
-- Or use http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:decodeFloat
-- TODO: It would also be nice to cite https://github.com/sydow/ireal/
-- TODO: It would be nice to make this code polymorphic (for any Fractional type?)
(~=) :: (Fractional a, Ord a) => a -> a -> Bool
a ~= b = abs (a - b) < 1.0e-10
infixl 4 ~=

--Faltning i diskret tid
convD :: (Num val, Time time)
         => Signal time val
         -> Signal time val
         -> [time]
         -> Signal time val
convD s0 s1 points = sum $ map conv (L.nub points)
  where conv n m = (s0 (n-m)) * (s1 m)

--Ett System kan betraktas som en funktion för signaler
type System time val = Signal time val

sample :: (Time time) => time -> time -> time -> [time]
sample from to samplesize = [from, step .. to]
  where step = from + samplesize


-- Genererar utsignalen för enkla signaler och system
outSignal :: (Num val, Time time) => System time val
          -> Signal time val
          -> [time]
          -> Signal time val
outSignal sys sig points = convD sys sig points

isLinear :: (Show val, Time time, Fractional val, Ord val) => Signal time val -- ^ Insignal 1
         -> Signal time val -- ^ Insignal 2
         -> System time val  -- ^ System
         -> val -- ^ Skalningsfaktor 1
         -> val -- ^ Skalningsfaktor 2
         -> [time] -- ^ Tiderna då vi mäter
         -> Bool
isLinear x0 x1 sys a b ts = testAt (y0' + y1') (y0 `scale` a + y1 `scale` b) ts
    where y0  = outSignal sys x0 ts
          y1  = outSignal sys x1 ts
          y0' = outSignal sys (x0 `scale` a) ts
          y1' = outSignal sys (x1 `scale` b) ts
          testAt a b t = and $ map (\t -> a t ~= b t) ts

-- | Kollar om ett system är linjärt genom att mata det med två signaler och
-- använder sig av superpositionsprincipen.
prop_isLinearCont :: (Show time, Show val, Time time, Q.Arbitrary time, Q.Arbitrary val, RealFrac val) => Signal time val -- ^ Insignal 1
                  -> Signal time val -- ^ Insignal 2
                  -> System time val -- ^ System
                  -> IO ()
prop_isLinearCont x0 x1 sys = Q.quickCheck (isLinear x0 x1 sys)

-- | Kollar om ett system är linjärt genom att mata det med två signaler och
-- använder sig av superpositionsprincipen.
prop_isLinearDisc :: (Show val, Time time, RealFrac val) => Signal time val -- ^ Insignal 1
                  -> Signal time val -- ^ Insignal 2
                  -> System time val -- ^ System
                  -> val -- ^ Skalfaktor 1 (genereras av QuickCheck)
                  -> val -- ^ Skalfaktor 2 (genereras av QuickCheck)
                  -> [time] -- ^ Tidspunkt (genereras av QuickCheck)
                  -> Bool
prop_isLinearDisc x0 x1 sys = \a b ts -> isLinear x0 x1 sys a b ts

--Övning: Implementera ett test för tidsinvariansegenskapen, givet timeshift
--Ett system är tidsinvariant om en tidsförskjutning i insignalen ger samma
--tidsförskjutning i utsignalen. Alltså:
--Xin(t-c) -> Xut(t-c), där c är en reell konstant.
isTimeInv :: (Time time, RealFrac time, RealFrac val) => Signal time val -> System time val -> [time] -> time -> Bool
isTimeInv x sys ts c = and $ map (testAt c) ts
  where x' = shift x c
        y  = outSignal sys x ts
        y' = outSignal sys x' ts
        testAt o t= y' t ~= (shift y o) t

--Ett system är ett LTI-system om det uppfyller superpositionsprincipen och är
--tidsinvariant
-- isLTICont :: Double -> Signal time val -> Double -> Signal time val -> System time val -> time -> time -> Bool
-- isLTICont a x b y sys t c = isLinear x y sys a b t && isTimeInvCont x sys t c

-- isLTIDisc :: Double -> Signal time val -> Double -> Signal time val -> System time val -> time -> time -> Bool
-- isLTIDisc a x b y sys t c = isLinear x y sys a b t && isTimeInvDisc x sys t c
