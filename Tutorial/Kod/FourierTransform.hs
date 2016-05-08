module FourierTransform where

import Expression
import ComplexNumbers

-- | Fourier transforms an Expression
transform :: Expression Complex -> Expression Complex
transform (Const a)   = Const a :*: Impulse
transform Pi          = Pi :*: Impulse
transform Id          = Id
transform Impulse     = 1
transform (e0 :+: e1) = transform e0 + transform e1
-- | Time shift property
transform (Id :-: Const a) = Exp (Const (-j) :*: Const a :*: Id)
-- | Frequency shift property
transform (Exp (Const j :*: Const omega :*: Id) :*: exp) = Shift omega (transform exp)
-- | Convolution theorem
transform (Conv e0 e1) = transform e0 :*: transform e1
transform (Exp (Const a :*: Id)) = Const 1 :/: (Const a :-: (Const j :*: Id))
transform (Const e0 :*: e1) = Const e0 * transform e1
transform (e0 :*: Const e1) = Const e1 * transform e0
transform exp         = error ("transform: The transform (" ++ show exp ++ ") is not yet implemented.")
