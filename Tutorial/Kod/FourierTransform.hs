module FourierTransform where

import Expression
import ComplexNumbers

-- För att göra transformationerna tydligare
type TimeExpression = Expression Complex
type FreqExpression = Expression Complex

-- TODO:
-- * fourier :: TimeExpression -> FreqExpression
-- * invFourier :: FreqExpression -> TimeExpression
-- * prop_fourierIdentity x = x == (invFourier . fourier) x

-- | Fouriertransform, transformerar ett uttryck från tidsdomän
--   till frekvensdomän
fourier :: TimeExpression -> FreqExpression
fourier Impulse                = 1
fourier (Const a)              = Const a * Impulse
fourier (Const a :*: Const b)  = Const (a*b) * Impulse
fourier Pi                     = Pi * Impulse
fourier Id                     = Id
fourier (Exp (Const a :*: Id)) = Const 1 :/: (Const a :-: (Const j :*: Id))
-- Tidsförskjutning
fourier (Shift t expr) = Exp (Const j * (Const (-t)) * Id) * fourier expr
-- Frekvensförskjutning
fourier (Exp (Const j :*: Const omega :*: Id) :*: expr) =
  Shift omega (fourier expr)
fourier (expr :*: Exp (Const j :*: Const omega :*: Id)) =
  Shift omega (fourier expr)
-- Linjäritet
fourier (Const a :*: e0) = Const a * fourier e0
fourier (e0 :*: Const a) = fourier e0 * Const a
fourier (e0 :+: e1) = fourier e0 + fourier e1
fourier (e0 :-: e1) = fourier e0 - fourier e1
-- Faltning (Convolution theorem)
fourier (e0 :*: e1) = Conv (fourier e0) (fourier e1)
fourier (Conv e0 e1) = fourier e0 * fourier e1
-- För odefinierade transformer
fourier exp = error ("fourier: Fouriertransformen för uttrycket (" ++
                     show exp ++ ") är ännu inte implementerad")

-- | Invers fouriertransform, transformerar ett funktionsuttryck från
--   frekvensdomän till tidsdomän.
invFourier :: FreqExpression -> TimeExpression
invFourier Impulse   = 1
invFourier (Const a) = Const a * Impulse
invFourier (Const a :*: Const b) = Const (a*b) * Impulse
invFourier Pi        = Pi * Impulse
invFourier Id        = Id
invFourier (Const 1 :/: (Const a :-: (Const j :*: Id))) = Exp (Const a :*: Id)
-- Tidsförskjutning
invFourier (Exp (Const j :*: Id :*: Const t0) :*: expr) =
  Shift (- t0) (invFourier expr)
invFourier (expr :*: Exp (Const j :*: Id :*: Const t0)) =
  Shift (- t0) (invFourier expr)
-- Frekvensförskjutning
invFourier (Shift omega0 expr) =
  invFourier expr * Exp (Const j * Const omega0 * Id)
-- Linjäritet
invFourier (Const a :*: e0) = Const a * invFourier e0
invFourier (e0 :*: Const a) = invFourier e0 * Const a

invFourier (e0 :+: e1) = invFourier e0 + invFourier e1
invFourier (e0 :-: e1) = invFourier e0 - invFourier e1
-- Faltning (Convolution theorem)
invFourier (e0 :*: e1) = Conv (invFourier e0) (invFourier e1)
invFourier (Conv e0 e1) = invFourier e0 * invFourier e1
-- För odefinierade transformer
invFourier exp = error ("invFourier: Inversa fouriertransformen för uttrycket (" ++ show exp ++ ") är ännu inte implementerad")

-- TODO: please add some property to enable checking these rules.
prop_fourierIdentity exp = exp == fId exp
  where fId = invFourier . fourier
