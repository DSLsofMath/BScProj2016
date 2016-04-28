-- | Module for numeric expressions
module Expression where

import FunNumInst
import ComplexNumbers

-- | Expression data type for numeric expressions
data Expression a =  Const a
                  |  Id
                  |  Exp (Expression a)
                  |  Expression a :+: Expression a
                  |  Expression a :*: Expression a
                  |  Expression a :/: Expression a
                  |  Conv (Expression a) (Expression a)
                  |  Log (Expression a)
                  |  Negate (Expression a)
                  |  Sin (Expression a)
                  |  Cos (Expression a)
                  |  Tan (Expression a)
                  |  Asin (Expression a)
                  |  Acos (Expression a)
                  |  Atan (Expression a)
                  |  Sinh (Expression a)
                  |  Cosh (Expression a)
                  |  Asinh (Expression a)
                  |  Acosh (Expression a)
                  |  Atanh (Expression a)
                  deriving (Show, Eq)

-- | Evaluates an Expression to a function
eval :: (Num a,Floating a) => Expression a -> (a -> a)
eval (Const a) = const a
eval Id        = id
eval (Exp e)   = exp . eval e

-- | Numeric instance of our expression data type
instance (Num a, Floating a) => Num (Expression a) where
      (+)          = (:+:)
      (*)          = (:*:)
      fromInteger  = Const . fromInteger
      negate       = Negate
      abs e        = undefined
      signum e     = undefined

-- | Fractional instance of our expression data type
instance (Floating a) => Fractional (Expression a) where
         fromRational = Const . fromRational
         (/)          = (:/:)

-- | Floating instance of our expression data type
instance (Floating a) => Floating (Expression a) where
         pi    = Const pi
         exp   = Exp
         log   = Log
         sin   = Sin
         cos   = Cos
         sinh  = Sinh
         cosh  = Cosh
         atan  = Atan
         atanh = Atanh
         asin  = Asin
         acos  = Acos
         asinh = Asinh
         acosh = Acosh

