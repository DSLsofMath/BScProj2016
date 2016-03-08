  Komplexa tal kan ses som ett par av reella värden.

> data Complex = Complex (Double, Double)
>    deriving Eq

  Där första komponenten representerar realdelen och den andra komponenten imaginärdelen.

> realPart :: Complex -> Double
> realPart (Complex c) = fst c

> imPart :: Complex -> Double
> imPart (Complex c) = snd c

> conjugate :: Complex -> Complex
> conjugate z = Complex (realPart z, negate (imPart z))

> printComplex :: Complex -> String
> printComplex z
>  | r == 0 = show im ++ "j"
>  | i == 0 = show r
>  | otherwise = show r ++ " + " ++ show im ++ "j"
>  where im = imPart z
>        r = realPart z

> instance Show Complex where
>    show = printComplex

> instance Num Complex where

  Plus är förhållandevist trivialt

>    z + w       = Complex (realPart z + realPart w, imPart z + imPart w)

  Multiplikationen följer ifrån att vi multiplicerar komponenterna parvis med varandra (i likhet med (a+b) * (c+d))

>    z * w       = Complex (realZ*realW - imZ*imW, realZ*imW + realW*imZ)
>       where realZ = realPart z
>             realW = realPart w
>             imZ   = imPart z
>             imW   = imPart w

  Negationen av ett komplext tal är ett komplext tal där båda komponenter är negerade

>    negate z    = Complex (negate $ realPart z, negate $ imPart z)

  Eftersom komplexa tal inte har någon definition av positiva och negativa tal är signum odefinierad.

>    signum z    = undefined

  Absolutbeloppet av ett komplext tal är pythagoras sats på dess komponenter.

>    abs z       = Complex (hyp, 0)
>      where hyp = sqrt (r*r + im*im)
>            r   = realPart z
>            im   = imPart z

  Heltal är bara komplexa tal utan imaginärdel

>    fromInteger n = Complex (fromInteger n, 0)
