-- | Numeric instance for numeric functions.
module FunNumInst where

instance Num to => Num (from -> to) where
         (+) f g       = \a -> f a + g a
         (*) f g       = \a -> f a * g a
         abs f         = abs . f
         signum f      = signum . f
         fromInteger   = const . fromInteger
         negate f      = negate . f

instance Fractional to => Fractional (from -> to) where
         fromRational  = const . fromRational
         recip f       = \a -> 1 / f a

instance Floating to => Floating (from -> to) where
         pi          = const pi
         exp         = (.) exp
         log         = (.) log
         sin         = (.) sin
         cos         = (.) cos
         asin        = (.) asin
         acos        = (.) acos
         atan        = (.) atan
         sinh        = (.) sinh
         cosh        = (.) cosh
         asinh       = (.) asinh
         acosh       = (.) acosh
         atanh       = (.) atanh
