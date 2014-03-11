{-
Programming Languages @ ITAM
authors:  Salvador Medina
          Gabriela Gongora
description:  This program implements operations over polynomials.
              Each polynomial is represented by an integer array
              where the exponents are implicit in the array position.
This works
-}
--Used for intercalate
import Data.List

--	Reduces polynomial to proper representation
toProper [] = []
toProper p = if (last p /= 0) -- this means there is nothing to do.
             then p 
             else toProper $ init p -- this means the last zero can be discarded.

--	Adds two polynomials
addPoly [] []     = []
addPoly (x:xs) [] = x : addPoly xs [] 
addPoly [] (y:ys) = y : addPoly [] ys
addPoly (x:xs) (y:ys) = x + y : addPoly xs ys

-- Substracts two polynomials
subPoly [] []     = []
subPoly (x:xs) [] = x : subPoly xs [] 
subPoly [] (y:ys) = y : subPoly [] ys
subPoly (x:xs) (y:ys) = x - y : subPoly xs ys

-- Multiplies a polynomial by x
multiplyByX p = 0:p   --Increase the whole polynomial by 1 degree

-- Multiplies polynomial by a constant
timesPoly a p1 = map (a*) p1

-- Multiply two polynomials
multPoly [] p2 = [] --if multiplied by 0 -> 0
multPoly (p:p1) p2 = let pTimesP2 = timesPoly p p2
                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1Timesp2  

-- Negates all the polynomial coefficients
negatePoly = map negate

-- Derives a polynomial
dxPoly [] = []
dxPoly (_:ps) = zipWith (*) ps [1..]

-- Show polynomial as a string
showPoly [] = show 0
showPoly p =  let cOs = zip p [0..]								-- Create (coeff, exp) tuples
                  nonZeroCOs = filter (\(c,_) -> c /= 0) cOs 	-- Keep the non-zero coefficients
                  cShow c = if c == 1 							-- This function is used to show the coefficient	
                            then "" 
                            else show c
                  nShow n = case n of 							-- This function is to show the exponent
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0 				-- Mix both show funcs
                               then show 1 
                               else intercalate " " $ filter (/="") [cShow c, nShow n]            
                  terms = map (\(c,n) -> cnShow c n) nonZeroCOs
              in intercalate " + " (reverse terms)

printPoly p = print $ showPoly $ p

main = do printPoly $ [1,3,1]
          printPoly $ addPoly [1,0,4,0,5] [1,2,1,3]
          printPoly $ subPoly [1,0,4,0,5] [1,2,1,3]
          printPoly $ timesPoly 4 [1,0,4,0,5]
          printPoly $ dxPoly  [1,0,4,0,5]
          printPoly $ multPoly [3,1] [4,1]
