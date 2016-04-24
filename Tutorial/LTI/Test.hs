-- | Test module for the LTI module
module LTI.Test where

import qualified Test.QuickCheck as Q
import LTI

-- Helping functions
convHelp x ts = and $ map (\t -> y t ~= x t) ts
  where y = convD x x ts

-- | Checks that convolution with two discrete unit impulse (kronecker delta)
-- results in a third kronecker delta function
prop_convId :: Time time => [time] -> Bool
prop_convId = convHelp discImpulse

-- | Checks that convolution with a time signal and unit impulse (kronecker delta)
-- results in a the same time signal as we started with.
prop_convIdC :: (Floating time, Time time, RealFrac val) => Signal time val -> [time] -> time -> Bool
prop_convIdC x ts t = testAt y x t
  where y = convD discImpulse x ts
        testAt x0 x1 t = x0 t ~= x1 t
