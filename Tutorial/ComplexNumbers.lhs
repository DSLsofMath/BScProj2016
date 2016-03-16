> module ComplexNumbers where

> import GHC.Real

  Komplexa tal kan ses som ett par av reella värden.

> data Complex = Complex (Double, Double)
>              deriving Eq

  Där första komponenten representerar realdelen och den andra komponenten imaginärdelen.

> realPart :: Complex -> Double
> realPart (Complex c) = fst c

> imPart :: Complex -> Double
> imPart (Complex c) = snd c

> conjugate :: Complex -> Complex
> conjugate z = Complex (realPart z, negate (imPart z))

TODO: kolla upp atan2: http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:atan2

> -- | Argumentet av ett komplext tal. Bökigt värre på grund av tråkiga kvadranter och bös.
> arg :: Complex -> Double
> arg z   | (imPart z) < 0  && (realPart z) < 0 = atan (imPart z / realPart z) - pi
>         | (realPart z) < 0                  = atan (imPart z / realPart z) + pi
>         | otherwise                         = atan (imPart z / realPart z)

TODO: Använd ett assymmetriskt namn för denna operator (eftersom den inte är kommutativ).

> (.*.) :: Double -> Complex -> Complex
> a .*. z = Complex (a * realPart z, a * imPart z)

> -- | j är det komplexa talet med realdelen 0 och imaginärdelen 1
> -- Många matematiktexter kallar detta talet också för `i`
> j :: Complex
> j = Complex (0, 1)

TODO: Försök få   read . printComplex = id
TODO: "name and reuse" (show im ++ "j")

> printComplex :: Complex -> String
> printComplex z
>  | r == 0 = show im ++ "j"
>  | im == 0 = show r
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

TODO: Försök få denna lag för signum att gälla: http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:signum

>    signum z    = undefined

  Absolutbeloppet av ett komplext tal är pythagoras sats på dess komponenter.

>    abs z       = Complex (hyp, 0)
>      where hyp = sqrt (r*r + im*im)
>            r   = realPart z
>            im   = imPart z

  Heltal är bara komplexa tal utan imaginärdel

>    fromInteger n = Complex (fromInteger n, 0)

> instance Fractional Complex where

  Division utförs genom att man förlänger hela bråket med nämnarens konjugat

>     z / w = Complex (realPart zw' / realPart ww', imPart zw' / realPart ww')
>         where zw' = z * (conjugate w)
>               ww' = w * (conjugate w)

  En kvot av två heltal är helt reell och därför kommer imaginärdelen vara 0

TODO: Bättre att göra som med (fromInteger n) ovan - det blir en effektivare beräkning.

>     fromRational z = fromInteger (numerator z) / fromInteger (denominator z)

  Garanterar att imaginärdelen alltid är 0 ifrån en fromRational

> prop_testRatioNoImpart :: Rational -> Bool
> prop_testRatioNoImpart ratio = imPart (fromRational ratio :: Complex) == 0

> instance Floating Complex where

  Det komplexa talet pi är ett tal med realdelen pi och imaginärdelen 0

>     pi    = Complex (pi, 0)

  Potenslagarna ger att e^(a+bj) <=> e^a * e^bj och
  Eulers formel ger att e^bj <=> cos b + jsin b

TODO: låt euler finns på toppnivån. Kan vara bra att ha.

>     exp z = exp (realPart z) .*. euler (imPart z)

  Skapar ett trigonometriskt komplext tal utifrån en vinkel

>         where euler phi = Complex (cos phi, sin phi)

  Eulers formel ger att man kan skriva om sin x som (e^jx - e^-jx)/2j
  Eftersom vi definerat exponentialfunktionen för komplexa tal kan vi använda
  eulers formel som vår sinus implementation för komplexa tal.

>     sin z = (exp (j*z) - exp (-(j*z)))/(2 .*. j)

  Eulers formel ger att man kan skriva om cos x som (e^jx + e^-jx)/2

>     cos z = (exp (j*z) + exp (-(j*z)))/2

>     cosh z = Complex (cosh (realPart z) * cos (imPart z), sinh (realPart z) * sin (imPart z))
>     sinh z = Complex (sin (realPart z) * cosh (imPart z), cos (realPart z) * sinh (imPart z))

  Eulers formel ger att z = re^(j*phi) och eftersom log och exponentialfunktionen
  är varandras inverser och logaritmlagarna ger ex. `log (a*b)` <=> `log a + log b`.
  Därför kan vi skriva log `z = log r + log (e^(j*phi))` = `log r + j*phi`

TODO: Något fel med ` ovan

>     log z = Complex (logR (abs z), (arg z))
>         where logR = log . realPart

Funktioner vi troligen / förhoppningsvis inte kommer behöva och som vi därför lämnar odefinierade än så länge.

>     atanh = undefined
>     atan  = undefined
>     asinh = undefined
>     asin = undefined
>     acosh = undefined
>     acos = undefined
