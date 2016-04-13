{-# TypeSynonymInstances #-}

--Tiden kan vara kontinuelig eller diskret, double eller int.
type ContTime = Double
type DiscTime = Integer

type Signal a b = (a -> b)

type ContSignal a = Signal ContTime a
type DiscSignal a = Signal DiscTime a

--Gäller för alla funktioner double -> double, t.ex. sin
type ContTimeFun = ContSignal Double
type DiscTimeFun = DiscSignal Int

--Kontinuerlig Impuls: Oändlig om t=0, annars 0
contImpulse :: ContTimeFun
contImpulse t | t == 0 = undefined
		      | otherwise = 0

--Diskret Impuls: 1 om t=0, annars 0
discImpulse :: DiscTimeFun
discImpulse t | t == 0 = 1
              | otherwise = 0

--Kontinuerligt enhetssteg: 0 om t<0, 1 om t >= 0
contStep :: ContTimeFun
contStep t | t < 0 = 0
--HalfMaximumConvention, ett vanligt sätt skriva för t=0
		   | t == 0 = 0.5
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



--Ett LTI-system består, här, av en specifik form av Expression.
--Måste innehålla en char, så att man kan stoppa in en signal i den.
-- data LTI = 	Expression



--multSignal :: Int -> Signal -> Signal
--multSignal i s = Signal i s

-- Genererar utsignalen för enkla signaler och system
-- outSignal :: Signal -> LTI -> Signal
-- outSignal inSignal LTI = undefined




--Övningar till LTI-kapitlet (Kräver att vi definierat en typ "Signal" mm.)
-- Vad har vi för olika egenskaper som vi kan motbevisa/bevisa?
-- Tidsinvarians
-- (Minneslöst(statiskt) eller system med minne(dynamiskt).)
-- Inverterbart(är varje output unikt för ett input?) "roten ur" = ej inverterbart ty sqrt(x²) = ±x, hitta inversen för systemet, koppla samman med systemet
-- och få sedan ut identiteten.


-- Övning 1: Implementera superpositionsprincipen som Quickcheck property.
-- Lösning:
--prop_super :: Int -> Signal -> Int -> Signal -> LTI -> Bool
--prop_super i1 s i2 s lti = 	OutSignal(i1 s, lti) == i1 OutSignal(s, lti) &&
--							OutSignal(i2 s, lti) == i2 OutSignal(s, lti)





--Övning 2: Implementera tidsinvariansegenskapen som Quickcheck property.
--Lösning:
--prop_tidInv ::