import Data.Char
import Test.QuickCheck
square :: Int -> Int
square x = x * x 
pyth :: Int -> Int -> Int
pyth x y = square x + square y
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square c == pyth a b
isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c = square c == pyth a b||square a == pyth b c||square b == pyth a c
halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x|x <- xs]

prop_halfEvens xs = map (\x -> if even x then x `div` 2 else x) xs == halfEvens xs
    where types = xs :: [Int]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs= [x|x<-xs,x>=a,x<=b]

prop_inRange a b xs = filter (\x -> x>=a && x<=b) xs == inRange a b xs

countPositives :: [Int] -> Int
countPositives xs = length[x|x<-xs,x>0]
capitalised :: String -> String
capitalised xs = [toUpper x|x<-xs, x == head xs]++[toLower x|x<-xs,x /= head xs]
lowercase :: String -> String
lowercase xs = [toLower x|x<-xs]
title :: [String] -> [String]
title xs = [capitalised x|x<-xs, x == head xs]++[if length x >=4 then capitalised x else lowercase x|x<-xs,x /= head xs]



