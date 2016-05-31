module FourierTransform where

import Expression
import ComplexNumbers

-- TODO:
-- * fourier :: TimeExpression -> FreqExpression
-- * invFourier :: FreqExpression -> TimeExpression
-- * prop_fourierIdentity x = x == (invFourier . fourier) x


-- | Fourier transforms an Expression
transform :: Expression Complex -> Expression Complex
transform (Const a)   = Const a :*: Impulse
transform Pi          = Pi :*: Impulse
transform Id          = Id
transform Impulse     = 1
transform (e0 :+: e1) = transform e0 + transform e1
transform (e0 :-: e1) = transform e0 - transform e1
-- TODO: x :-: y == x :+: (negate y) which means transform (x :-: y) == transform x :-: transform y
-- TODO: To exppress time shift you need another operation on Expressions. Is that what Shift is for?
-- | Time shift property
transform (Id :-: Const a) = Exp (Const (-j) :*: Const a :*: Id)
-- | Frequency shift property
transform (Exp (Const j :*: Const omega :*: Id) :*: exp) = Shift omega (transform exp)
-- | Convolution theorem
transform (Conv e0 e1) = transform e0 :*: transform e1
transform (Exp (Const a :*: Id)) = Const 1 :/: (Const a :-: (Const j :*: Id))
transform (Const e0 :*: e1) = Const e0 * transform e1
transform (e0 :*: Const e1) = Const e1 * transform e0
transform (e0 :*: e1) = Conv (transform e0) (transform e1)
transform exp         = error ("transform: The transform (" ++ show exp ++ ") is not yet implemented.")

-- TODO: please add some property to enable checking these rules.
