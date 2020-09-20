module Lib
  ( euler1,
    euler2,
    euler3,
  )
where

import Data.List

-- Euler 1
-- Find the sum of all the multiples of 3 or 5 below 1000
euler1 = sum ([i | i <- [1 .. 999], i `mod` 3 == 0 || i `mod` 5 == 0])

-- better: euler1 = sum (union [3,6..999] [5,10..999])

-- Euler 2
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued fibonacci terms.
euler2 = sum (takeWhile (<= 4000000) [i | i <- fibs, even i])

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs = [fib i | i <- [1 ..]]

-- better:
-- sum [ x | x <- takeWhile (<= 4000000) fibs, even x]
--  where
--   fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Euler 3
-- What is the largest prime factor of the number 600851475143?
euler3 = largestPrimeFactor (600851475143)

largestPrimeFactor x = head $ sortBy (\a b -> compare b a) $ factorize (x)

factorize factVal =
  case facts of
    [] -> [factVal]
    _ -> facts ++ factorize (factVal `div` head (facts))
  where
    facts = take 1 $ [n | n <- [2 .. round $ sqrt (fromIntegral factVal)], factVal `mod` n == 0]

factorizeRec factVal test facts
  | fromIntegral (test) > sqrt (fromIntegral factVal) = factVal : facts
  | factVal `mod` test == 0 = factorizeRec (factVal `div` test) 2 (test : facts)
  | otherwise = factorizeRec factVal (test + 1) facts