module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Manuel Stapff y Nicolas Piriz
-- Números: 303636 y 310896
----------------------------------------------------

import Prelude
import Data.List (nub)

-- EJERCICIO 1.1 --
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Show, Eq)
data BC = And | Or | Imp | Iff
  deriving (Show, Eq)
  
  
-- EJERCICIO 1.2 --
--a)
p:: L
p = V "p"

q :: L
q = V "q"

r :: L
r = V "r"

fa :: L
fa = Bin (p) And (Neg (Neg (q)))
--b)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))
--c)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))
--d)
fd :: L
fd = Bin (Neg (Bin r Imp r)) And (Bin (Neg (Neg p)) Or (Neg (Bin q And p)))


-- EJERCICIO 1.3 --
--a)
cantBin :: L -> Int
cantBin (V x) = 0
cantBin (Neg formula) = cantBin formula
cantBin (Bin a x b) = (cantBin a) + (cantBin b) + 1 

--b)
valores :: L -> [(Var,Bool)]
valores (V x) = [(x, True)]
valores (Neg (V x)) = [(x, False)]
valores (Bin a x b) = (valores a) ++ (valores b) 
--c)
dobleNeg :: L -> L
dobleNeg (V x) = V x
dobleNeg (Neg (Neg (x))) = dobleNeg x 
dobleNeg (Neg x) = Neg (dobleNeg x)
dobleNeg (Bin a x b) = Bin (dobleNeg a) x (dobleNeg b)
--d)
cambiar :: L -> L
cambiar (V x) = V x
cambiar (Neg x) = Neg (cambiar x)
cambiar (Bin a Or b) = Bin (Neg (cambiar a)) Imp (cambiar b)
cambiar (Bin a x b) = Bin (cambiar a) x (cambiar b)

--e)
cantPropX :: L -> Var -> Int
-- cantPropX = undefined
cantPropX (V x) y | x == y = 1
                  | otherwise = 0
cantPropX (Neg x) (p) = cantPropX x p
cantPropX (Bin a x b) (p) = (cantPropX a p) + (cantPropX b p)


--f)
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg form) = nub (listarProp form)
listarProp (Bin a x b) = nub ((listarProp a) ++ (listarProp b)) 

--usar nub dijeron en clase

--g)
sustCon :: L -> BC -> BC -> L
sustCon (V x) bin1 bin2 = (V x)
sustCon (Neg form) bin1 bin2 = Neg (sustCon form bin1 bin2)
sustCon (Bin a x b) bin1 bin2 | x == bin1 = Bin (sustCon a bin1 bin2) bin2 (sustCon b bin1 bin2)
                              | otherwise = Bin (sustCon a bin1 bin2) x (sustCon b bin1 bin2)
--h)
swapCon :: L -> BC -> BC -> L
swapCon (V x) bin1 bin2 = (V x)
swapCon (Neg form) bin1 bin2 = Neg (swapCon form bin1 bin2)
swapCon (Bin a x b) bin1 bin2 | x == bin1 = Bin (swapCon a bin1 bin2) bin2 (swapCon b bin1 bin2)
                              | x == bin2 = Bin (swapCon a bin1 bin2) bin1 (swapCon b bin1 bin2)
                              | otherwise = Bin (swapCon a bin1 bin2) x (swapCon b bin1 bin2)

--i)
invertir :: L -> L
invertir = undefined

--j)
sustSimp :: Var -> L -> L -> L
sustSimp = undefined

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult = undefined