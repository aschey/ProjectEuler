module Lib
    ( 
    euler1,
    euler2,
    euler3
    ) where
import Data.List
-- Euler 1
euler1 :: Int
euler1 = sum([i | i <- [1..999], i `mod` 3 == 0 || i `mod` 5 ==0 ])

-- Euler 2
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fibs :: [Int]
fibs = [fib i | i <- [1..]]
euler2 :: Int
euler2 = sum(takeWhile (<= 4000000) [i | i <- fibs, even i])

-- Euler 3
euler3 :: Int -> Int
euler3 x = head $ sortBy(\a b -> compare b a) $ factorize(x)    

factorize :: Int -> [Int]
--factorize factVal = factorizeRec factVal 2 []

factorize factVal = 
    case facts of
        [] -> [factVal]
        _  -> facts ++ factorize(factVal `div` head(facts))
    where facts = take 1 $ [n | n <- [2 .. round $ sqrt(fromIntegral factVal)], factVal `mod` n == 0]

factorizeRec :: Int -> Int -> [Int] -> [Int]
factorizeRec factVal test facts
    | fromIntegral(test) > sqrt(fromIntegral factVal) = factVal: facts
    | factVal `mod` test == 0 = factorizeRec (factVal `div` test) 2 (test: facts)
    | otherwise = factorizeRec factVal (test + 1) facts