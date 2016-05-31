-- | Module for numeric expressions
module Expression where

import FunNumInst
import ComplexNumbers

-- | Expression data type for numeric expressions
data Expression a =  Const a
                  |  Id
                  |  Pi
                  |  Impulse
                  |  Exp (Expression a)
                  |  Expression a :+: Expression a
                  |  Expression a :-: Expression a
                  |  Expression a :*: Expression a
                  |  Expression a :/: Expression a
                  |  Conv (Expression a) (Expression a)
                  |  Shift a (Expression a)
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
                  deriving (Show,Eq)

-- | Evaluates an Expression to a function
eval :: (Num a, Floating a, Eq a) => Expression a -> (a -> a)
eval (Const a)   = const a
eval Id          = id
eval Pi          = const pi
eval (Impulse)   = \t -> if t == 0
                            then 1 -- TODO: I would suggest 1/0 (Inf) or 1/eps where eps is the smallest double greater than zero
                            else 0
eval (Exp e)     = exp . eval e
eval (e0 :+: e1) = eval e0 + eval e1
eval (e0 :-: e1) = eval e0 - eval e1
eval (e0 :*: e1) = eval e0 * eval e1
eval (e0 :/: e1) = eval e0 / eval e1
eval (Log e)     = log . eval e
eval (Negate e)  = negate . eval e
eval (Sin e)     = sin . eval e
eval (Cos e)     = cos . eval e
eval (Tan e)     = tan . eval e
eval (Asin e)    = asin . eval e
eval (Acos e)    = acos . eval e
eval (Sinh e)    = sinh . eval e
eval (Cosh e)    = cosh . eval e
eval (Asinh e)   = asinh . eval e
eval (Acosh e)   = acosh . eval e
eval (Atanh e)   = atanh . eval e
eval (Shift offset exp) = \t -> (eval exp) (t - offset)

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
         pi    = Pi
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

-- -- | Show instance for Showable Expressions
-- instance Show a => Show (Expression a) where
--          show = showE

-- showE :: (Show a) => Expression a -> String
-- showE exp = "f(var) = " ++ showExp exp

showExp :: (Show a) => Expression a -> String
showExp (Const a)   = show a
showExp Id          = "id"
showExp Pi          = "π"
showExp Impulse     = "δ"
showExp (Exp e)     = "e^(" ++ showExp e ++ ")"
showExp (e0 :*: e1) = showCompExps e0 ++ "" ++ showCompExps e1
showExp (e0 :+: e1) = showCompExps e0 ++ " + " ++ showCompExps e1
showExp (e0 :-: e1) = showCompExps e0 ++ " - " ++ showCompExps e1
showExp (e0 :/: e1) = showCompExps e0 ++ " / " ++ showCompExps e1
showExp (Conv e0 e1) = showCompExps e0 ++ " * " ++ showCompExps e1
showExp (Shift offset exp) = showExpWithOffset offset exp

-- | Composed Expressions needs to be enclosed in a pair of parens
showCompExps :: (Show a) => Expression a -> String
showCompExps (e0 :+: e1) = "(" ++ showCompExps e0 ++ " + " ++ showCompExps e1 ++ ")"
showCompExps (e0 :-: e1) = "(" ++ showCompExps e0 ++ " - " ++ showCompExps e1 ++ ")"
showCompExps (e0 :*: e1) =  "(" ++ showCompExps e0 ++ ")(" ++ showCompExps e1 ++ ")"
showCompExps (e0 :/: e1) = "(" ++ showCompExps e0 ++ " / " ++ showCompExps e1 ++ ")"
showCompExps exp         = showExp exp

showExpWithOffset :: (Show a) => a -> Expression a -> String
showExpWithOffset offset Id      = "(var - " ++ show offset ++ ")"
showExpWithOffset offset Impulse = "δ(var - " ++ show offset ++ ")"
showExpWithOffset _ Pi           = "π"
showExpWithOffset _ (Const a)    = show a
showExpWithOffset offset exp     = showExp (traverseExpE (Shift offset) exp)

traverseExpE e (e0 :*: e1) = (e e0) :*: (e e1)
traverseExpE e (e0 :+: e1) = (e e0) :+: (e e1)
traverseExpE e (e0 :-: e1) = (e e0) :-: (e e1)
traverseExpE e (e0 :/: e1) = (e e0) :/: (e e1)
traverseExpE e exp         = e exp

-- | Minimerar en Expression datatyp tills två minimeringar returernar
-- samma värde
minE :: (Num a, Eq a) => Expression a -> Expression a
minE e
     | e == e' = e
     | otherwise = minE e'
     where e' = minE' e

-- | Minimerar en Expression-datatyp genom att ta bort onödiga termer.
minE' :: (Num a, Eq a) => Expression a -> Expression a
minE' (e :*: Const 1) = minE' e
minE' (Const 1 :*: e) = minE' e
minE' (e0 :*: e1) = minE' e0 :*: minE' e1
minE' (e :/: Const 1) = minE' e
minE' (Const 1 :/: e) = minE' e
minE' (e0 :/: e1) = minE' e0 :/: minE' e1

minE' (e :+: Const 0) = minE' e
minE' (Const 0 :+: e) = minE' e
minE' (e0 :+: e1) = minE' e0 :+: minE' e1
minE' (e :-: Const 0) = minE' e
minE' (Const 0 :-: e) = minE' e
minE' (e0 :-: e1) = minE' e0 :-: minE' e1

minE' (Shift 0 e) = minE' e
minE' (Exp (Const 0)) = Const 1
minE' expr = expr
