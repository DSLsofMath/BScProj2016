{-# LANGUAGE FlexibleInstances #-}

module LTI where
import qualified Test.QuickCheck as Q

--Ett System kan betraktas som en funktion för signaler
type DiscSystem = DiscTimeFun
type ContSystem = ContTimeFun

timeShift :: Num a => Signal a b -> a -> Signal a b
timeShift sig o = \t -> sig (t - o)

--multSignal :: Int -> Signal -> Signal
--multSignal i s = Signal i s

-- Genererar utsignalen för enkla signaler och system
--discOutSignal :: DiscSystem -> DiscTimeFun -> DiscTimeFun
--discOutSignal sys insignal = discConvolution insignal sys 100

--contOutSignal :: ContSystem -> ContTimeFun -> ContTimeFun
--contOutSignal sys insignal = contConvolution insignal sys 100 0.1

