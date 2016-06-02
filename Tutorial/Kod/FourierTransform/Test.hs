-- | Tester för FourierTransform modulen
module FourierTransform.Test where

import FourierTransform

prop_fourierIdentity exp = exp == fId exp
  where fId = invFourier . fourier
