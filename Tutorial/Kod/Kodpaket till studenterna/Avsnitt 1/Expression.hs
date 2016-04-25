module Expression where

import Data.Function
import FunNumInst
import ComplexNumbers


data Expression =  Const Double
                |  Id
                |  Pi
                |  Exp Expression
                |  Expression :+: Expression
                |  Expression :*: Expression
                |  Expression :/: Expression
                |  Expression :°: Expression
                |  Expression :**: Expression
                |  Negate Expression
                |  Log Expression
                |  Sin Expression
                |  Cos Expression
                |  Tan Expression
                |  Asin Expression
                |  Acos Expression
                |  Atan Expression
                |  Sinh Expression
                |  Cosh Expression
                |  Asinh Expression
                |  Acosh Expression
                |  Atanh Expression
                deriving (Show, Eq)

eval  ::  Expression -> Double -> Double
eval expr = let funEval f e1 e2 = on f eval e1 e2
	        in case expr of
            e0 :+: e1   -> funEval (+) e0 e1
            e0 :*: e1   -> funEval (*) e0 e1
            e0 :°: e1   -> funEval (.) e0 e1
            e0 :/: e1   -> funEval (/) e0 e1
            e0 :**: e1  -> funEval (**) e0 e1
            Id          -> id
            Pi          -> const pi
            Const e     -> const e
            Negate e    -> negate . eval e
            Exp e       -> exp . eval e
            Log e       -> log . eval e
            Sin e       -> sin . eval e
            Cos e       -> cos . eval e
            Asin e      -> asin . eval e
            Acos e      -> acos . eval e
            Atan e      -> atan . eval e
            Sinh e      -> sinh . eval e
            Cosh e      -> cosh . eval e
            Asinh e     -> asinh . eval e
            Acosh e     -> acosh . eval e
            Atanh e     -> atanh . eval e


