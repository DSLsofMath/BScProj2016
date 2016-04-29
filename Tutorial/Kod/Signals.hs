{-# LANGUAGE GADTs #-}

{-
   Based on the Signal.Deep module in
   github.com/DSLsofMath/DSLsofMath/Lectures/07/src/Signal and adapted
   to polymorphic time
   -}

-- | Deep embedding of signals
module Signals where

-- | Deep signal data type
data Signal time val where
     ConstS :: val -> Signal time val
     TimeS  :: Signal time time
     MapT   :: (time -> time) -> Signal time val -> Signal time val
     (:$$)  :: Signal time (a -> b) -> Signal time a -> Signal time b

-- | Dummy type class to force time types to be numeric and comparable
class (Num time, Eq time, Ord time) => Time time where

instance Time Double where

instance Time Integer where

-- * Constructors
-- | Constant signal.
constS :: (Time time) => a -> Signal time a
constS = ConstS

-- | The time signal.
timeS  :: (Time time) => Signal time time
timeS = TimeS

-- * Combinators
-- | Function application on time signals
($$) :: Signal time (a -> b) -> Signal time a  -> Signal time b
fs $$ xs = fs :$$ xs

-- | Transforming the time of a time signal.
mapT :: Time time => (time -> time) -> Signal time val -> Signal time val
mapT = MapT

-- | Transforming the value of a time signal.
mapS :: (Time time) => (a -> b) -> Signal time a -> Signal time b
mapS f xs = constS f $$ xs

-- | Sampling a signal at a given time point.
sample :: Signal time a -> time -> a
sample (ConstS x) = const x
sample TimeS      = id
sample (f :$$ s)  = \t -> sample f t $ sample s t
sample (MapT f s) = sample s . f

-- | Creates a time signal based on a numeric / floating function.
mkSig :: (Time time, Floating time) => (time -> time) -> (time -> time) -> Signal time time
mkSig f freq = mapT freq (mapS sin timeS)
