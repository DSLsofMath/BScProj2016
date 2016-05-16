module ComplexNumbers where
import GHC.Real

-- Komplexa tal kan ses som ett par av reella värden.

data Complex = Complex Double Double
    deriving Eq

-- Där första komponenten representerar realdelen och den andra komponenten
-- imaginärdelen.

realPart :: Complex -> Double
realPart (Complex re im) = re

imPart :: Complex -> Double
imPart = undefined

conjugate :: Complex -> Complex
conjugate z = Complex (realPart z) (negate (imPart z))

{-
   Argumentet av ett komplext tal.
   Bökigt värre på grund av tråkiga kvadranter och bös,
   men genom att använda funktionen atan2 så blir det mycket snyggare!
-}

arg :: Complex -> Double
arg z = atan2 (imPart z) (realPart z)

{-
   Utan atan2 hade vi fått skriva så här:

   arg :: Complex -> Double
   arg z  | (imPart z) < 0  && (realPart z) < 0 = atan (imPart z / realPart z) - pi
          | (realPart z) < 0                    = atan (imPart z / realPart z) + pi
          | otherwise                           = atan (imPart z / realPart z)

-}

scale :: Double -> Complex -> Complex
scale a z = Complex (a * realPart z) (a * imPart z)

{-
   j är det komplexa talet med realdelen 0 och imaginärdelen 1
   Många matematiktexter kallar detta talet också för `i`
-}

j :: Complex
j = Complex 0 1

instance Show Complex where
    show = printComplex

{-
   | Printfunktionen för complexa tal
   Exempel på hur utskriften bör funka:
   Complex 1 1    <-> 1.0 + 1.0j
   Complex 0 1    <-> 1.0j
   Complex 1 0    <-> 1.0
   Complex 1 (-1) <-> 1.0 - 1.0j
-}
printComplex :: Complex -> String
printComplex = undefined

-- | Skapar ett komplext tal utifrån en vinkel.
euler :: Double -> Complex
euler phi = Complex (cos phi) (sin phi)


instance Num Complex where
-- Plus är förhållandevist trivialt
  z + w = undefined
-- Multiplikationen följer ifrån att vi multiplicerar komponenterna
-- parvis med varandra (i likhet med (a+b) * (c+d))
  z * w = Complex (realZ*realW - imZ*imW) (realZ*imW + realW*imZ)
    where realZ = realPart z
          realW = realPart w
          imZ   = imPart z
          imW   = imPart w

-- Negationen av ett komplext tal är ett komplext tal där båda komponenter är negerade

  negate z = undefined

-- Den naturliga generaliseringen av signum från reella till komplexa tal:

  signum z = z / abs z

-- Absolutbeloppet av ett komplext tal är pythagoras sats på dess komponenter.

  abs z = undefined

-- Heltal är bara komplexa tal utan imaginärdel

  fromInteger n = Complex (fromInteger n) 0

instance Fractional Complex where
-- Division utförs genom att man förlänger hela bråket med nämnarens konjugat
  z / w = Complex (realPart zw' / realPart ww') (imPart zw' / realPart ww')
    where zw' = z * (conjugate w)
          ww' = w * (conjugate w)

-- En kvot av två heltal är helt reell och därför kommer imaginärdelen vara 0
  fromRational z = Complex re 0
    where re = fromInteger (numerator z) / fromInteger (denominator z)

instance Floating Complex where
-- Det komplexa talet pi är ett tal med realdelen pi och imaginärdelen 0
  pi = Complex pi 0

{-
   Potenslagarna ger att e^(a+bj) <=> e^a * e^bj och
   Eulers formel ger att e^bj <=> cos b + jsin b
-}
  exp z = scale (exp (realPart z)) (euler (imPart z))

{-
   Eulers formel ger att man kan skriva om sin x som (e^jx - e^-jx)/2j
   Eftersom vi definerat exponentialfunktionen för komplexa tal kan vi använda
   eulers formel som vår sinus implementation för komplexa tal
-}
  sin z = (exp (j*z) - exp (-(j*z)))/(scale 2 j)

-- Eulers formel ger att man kan skriva om cos x som (e^jx + e^-jx)/2
  cos z = (exp (j*z) + exp (-(j*z)))/2

  cosh z = Complex (cosh (realPart z) * cos (imPart z))
                   (sinh (realPart z) * sin (imPart z))

  sinh z = Complex (sin (realPart z) * cosh (imPart z))
                   (cos (realPart z) * sinh (imPart z))

{-
   Eulers formel ger att z = re^(j*phi) och eftersom log och exponentialfunktionen
   är varandras inverser och logaritmlagarna ger ex. `log (a*b)` <=> `log a + log b`.
   Därför kan vi skriva log `z = log r + log (e^(j*phi))` = `log r + j*phi`
-}
  log z = Complex (logR (abs z)) (arg z)
    where logR = log . realPart

{-
  Funktioner vi troligen / förhoppningsvis inte kommer behöva och som vi därför lämnar
  odefinierade än så länge.
-}
  atanh = undefined
  atan  = undefined
  asinh = undefined
  asin  = undefined
  acosh = undefined
  acos  = undefined
