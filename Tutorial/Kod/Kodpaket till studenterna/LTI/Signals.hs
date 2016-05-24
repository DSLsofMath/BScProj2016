{-# LANGUAGE FlexibleInstances #-}

module Signals where
import qualified Test.QuickCheck as Q

-- Tiden kan vara kontinuelig eller diskret, double eller int.
type ContTime = Double
type DiscTime = Integer

-- Skalet för en generell signal
type Signal a b = (a -> b)

-- Skalet för en kontinuerlig respektive diskret signal
type ContSignal a = Signal ContTime a
type DiscSignal a = Signal DiscTime a

-- En "färdig" signal, dvs en kontinuerlig eller diskret funktion
-- Gäller för alla funktioner double -> double, t.ex. sin och cos
type ContTimeFun = ContSignal Double
type DiscTimeFun = DiscSignal Double

-- Diskret Impuls: 1 om n=0, annars 0
discImpulse :: DiscTimeFun
discImpulse n = undefined

-- Diskret enhetssteg: 0 om n<0, 1 om n >= 0.
discStep :: DiscTimeFun
discStep n = undefined

-- Definierar vanliga beräkningoperationer, som t.ex. + och *, för Signaler
instance Num b => Num (Signal a b) where
    s0 + s1     = \a -> (s0 a) + (s1 a)
    s0 * s1     = \a -> (s0 a) * (s1 a)
    negate s    = \a -> negate (s a)
    abs s       = \a -> abs (s a)
    signum s    = \a -> signum (s a)
    fromInteger = const . fromInteger

-- | Skalar en signal/ökar amplituden med en konstant
scale :: Num b => Signal a b -> b -> Signal a b
scale sig f = (*f) . sig
infixl 7 `scale`

-- En jämförelsefunktion som tillåter väldigt små fel, kan användas för att
-- undvika fel som beror på avrundning
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 1.0e-10
infixl 4 ~=

-- | Undersöker om en signal har en specifik period. För enkelhetens skull antar vi
-- här att signalen är fortsatt periodisk om den är periodisk i 2 perioder.
contHasPeriod :: ContTimeFun -> ContTime -> Double -> Bool
contHasPeriod signal step period = and $ map () [0, step .. period]
    where hasPeriod signal period t = signal t ~= signal (t+period)

-- | Signalen är jämn om X(-t) = X(t).
contIsEven :: ContTimeFun -> ContTime -> Double -> Bool
contIsEven signal step limit = undefined

-- | Signalen är udda om X(-t) = X(t)
contIsOdd :: ContTimeFun -> ContTime -> Double -> Bool
contIsOdd signal step limit = and $ map testPair $ zip xs ys
    where testPair (a,b) = a ~= (-b)
          xs = map signal [0, step .. limit]
          ys = map signal [0,-step .. (-limit)]
