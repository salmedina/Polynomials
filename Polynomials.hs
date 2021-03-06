{-
Programming Languages @ ITAM
authors:  Salvador Medina
          Gabriela Gongora
description:  This program implements operations over polynomials.
              Each polynomial is represented by an integer array
              where the exponents are implicit in the array position.
-}

--Used for intercalate
import Data.List (intercalate)

--  Reduces polynomial to proper representation
toProper [] = []
toProper p = if (last p /= 0) -- this means there is nothing to do.
             then p 
             else toProper $ init p -- this means the last zero can be discarded.

-- Negates all the polynomial coefficients
negatePoly = map negate

--  Adds two polynomials
addPoly [] []     = []
addPoly (x:xs) [] = x : addPoly xs [] 
addPoly [] (y:ys) = y : addPoly [] ys
addPoly (x:xs) (y:ys) = x + y : addPoly xs ys

-- Substracts two polynomials
subPoly p q     = addPoly p (negatePoly q)

-- Multiplies a polynomial by x
multiplyByX p = 0:p   --Increase the whole polynomial by 1 degree

-- Multiplies polynomial by a constant
timesPoly c p1 = map (c*) p1

-- Multiply two polynomials
multPoly [] p2 = [] --if multiplied by 0 -> 0
multPoly (p:p1) p2 = let pTimesP2 = timesPoly p p2
                         xTimesP1TimesP2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1TimesP2  

-- Get the polynomial up to the power of n
powPoly :: (Eq a,Num a)=> [a] -> a ->[a]
powPoly p 0 = [1]
powPoly p n = multPoly p (powPoly p (n-1))

-- Evaluate polynomials
evalPoly p x = [sum (zipWith (*) p [x^n | n<- [0 ..]])]

-- Composite polynomials
compPoly p q =  let terms = (zipWith (timesPoly) p [powPoly q n | n <- [0 ..]])
                in foldr (addPoly) (head terms) (tail terms)

-- Derives a polynomial
dxPoly [] = []
dxPoly (_:ps) = zipWith (*) ps [1..]

-- Show polynomial as a string
showPoly [] = show 0
showPoly p =  let cOs = zip p [0..]               -- Create (coeff, exp) tuples
                  nonZeroCOs = filter (\(c,_) -> c /= 0) (reverse cOs)  -- Keep the non-zero coefficients
                  cShow c 
                        | c == 1 = ""               -- This function is used to show the coefficient   
                        | c > 1 = "+ "++show c
                        | c < 0 = "- "++show (c*(-1))
                  sShow s 
                        | s == 1 = ""               -- This function is used to show the coefficient   
                        | otherwise = show s
                  nShow n = case n of               -- This function is to show the exponent
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0        -- Mix both show funcs
                               then "+ 1" 
                               else intercalate "" $ filter (/="") [cShow c, nShow n]            
                  terms = ((sShow$(fst$head nonZeroCOs))++(nShow (snd$head nonZeroCOs))):(map (\(c,n) -> cnShow c n) (tail  nonZeroCOs))
              in intercalate " " (terms)

main = do let p = [1,2,3,4]
              q = [5, 0 ,3] 
          putStrLn $ "zero(x)        = " ++ showPoly []
          putStrLn $ "p(x)           = " ++ showPoly p
          putStrLn $ "p(x)           = " ++ showPoly q
          putStrLn $ "p(x)+q(x)      = " ++ showPoly (addPoly p q)
          putStrLn $ "p(x)*q(x)      = " ++ showPoly (multPoly p q)
          putStrLn $ "p(q(x))        = " ++ showPoly (compPoly p q)
          putStrLn $ "0-p(x)         = " ++ showPoly (subPoly [] p)
          putStrLn $ "p(3)           = " ++ showPoly (evalPoly p 3)
          putStrLn $ "p'(x)          = " ++ showPoly (dxPoly p)
          putStrLn $ "p''(x)         = " ++ showPoly (dxPoly (dxPoly p))