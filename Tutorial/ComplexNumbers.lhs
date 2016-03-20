> module ComplexNumbers where
> import GHC.Real

Komplexa tal kan ses som ett par av reella värden.

> data Complex = Complex (Double, Double)
>     deriving Eq

Där första komponenten representerar realdelen och den andra komponenten imaginärdelen.

> realPart :: Complex -> Double
> realPart (Complex c) = fst c

> imPart :: Complex -> Double
> imPart (Complex c) = snd c

> conjugate :: Complex -> Complex
> conjugate z = Complex (realPart z, negate (imPart z))

Argumentet av ett komplext tal. Bökigt värre på grund av tråkiga kvadranter och bös, men genom att använda funktionen atan2 så blir det mycket snyggare!

> arg :: Complex -> Double
> arg z = atan2 (imPart z) (realPart z)

> scale :: Double -> Complex -> Complex
> scale a z = Complex (a * realPart z, a * imPart z)

Behövs kanske inte då man kan skriva z / Complex (a,0)

(./.) :: Complex -> Double -> Complex
z ./. a = Complex (realPart z / a, imPart z / a)

j är det komplexa talet med realdelen 0 och imaginärdelen 1
Många matematiktexter kallar detta talet också för `i`

> j :: Complex
> j = Complex (0, 1)

TODO: Försök få   read . printComplex = id genom att göra printComplex enklare
TODO: "name and reuse" something like showIm im = show im ++ "j" to make it easy to change the spacing (or change "j" to "i", or the type from String to ShowS) in only one place

> printComplex :: Complex -> String
> printComplex z
>   | r == 0    = imj
>   | im == 0   = show r
>   | im < 0    = show r ++ " - " ++ show (abs im) ++ "j"
>   | otherwise = show r ++ " + " ++ imj
>     where im  = imPart z
>           r   = realPart z
>           imj = show im ++ "j"

> instance Show Complex where
>     show = printComplex

instance Read Complex where
    read = readComplex

> instance Num Complex where

Plus är förhållandevist trivialt

>     z + w           = Complex (realPart z + realPart w, imPart z + imPart w)

Multiplikationen följer ifrån att vi multiplicerar komponenterna parvis med varandra (i likhet med (a+b) * (c+d))

>     z * w           = Complex (realZ*realW - imZ*imW, realZ*imW + realW*imZ)
>         where realZ = realPart z
>               realW = realPart w
>               imZ   = imPart z
>               imW   = imPart w

Negationen av ett komplext tal är ett komplext tal där båda komponenter är negerade

>     negate z        = Complex (negate $ realPart z, negate $ imPart z)

Den naturliga generaliseringen av signum från reella till kompelxa tal:

>     signum z        = z / abs z

Absolutbeloppet av ett komplext tal är pythagoras sats på dess komponenter.

>     abs z           = Complex (hyp, 0)
>         where hyp   = sqrt (r*r + im*im)
>               r     = realPart z
>               im    = imPart z

Heltal är bara komplexa tal utan imaginärdel

>     fromInteger n   = Complex (fromInteger n, 0)

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
TODO: Bättre att göra som med (fromInteger n) ovan - det blir en effektivare beräkning.

>     exp z = scale (exp (realPart z)) (euler (imPart z))
>         -- Skapar ett trigonometriskt komplext tal utifrån en vinkel
>         where euler phi = Complex (cos phi, sin phi)

Eulers formel ger att man kan skriva om sin x som (e^jx - e^-jx)/2j
Eftersom vi definerat exponentialfunktionen för komplexa tal kan vi använda
eulers formel som vår sinus implementation för komplexa tal

>     sin z = (exp (j*z) - exp (-(j*z)))/(scale 2 j)

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

Funktioner vi troligen / förhoppningsvis inte kommer behöva och som vi därför lämnar
odefinierade än så länge.

>     atanh = undefined
>     atan  = undefined
>     asinh = undefined
>     asin  = undefined
>     acosh = undefined
>     acos  = undefined
