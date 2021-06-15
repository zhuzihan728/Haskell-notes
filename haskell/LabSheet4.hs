import Data.List
import Test.QuickCheck
import Data.Char
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  


spaceNum :: Int -> String -> Int
spaceNum a [] = 0
spaceNum a (x:xs) = if isSpace x then a else spaceNum (a+1) xs

getridspace :: String -> String
getridspace (x:xs) = if isSpace x then xs else (x:xs)

myWords :: String -> [String]
myWords [] = []
myWords [x] = if isSpace x then [] else [x]:[]
myWords ys = if (spaceNum 0 xs) == 0 then xs:[] else take (spaceNum 0 xs) xs : myWords (drop ((spaceNum 0 xs)+1) xs)
  where xs = getridspace ys

prop_mywords xs = myWords xs == words xs
  where types = xs :: String

myUnwords :: [String] -> String
myUnwords [] = []
myUnwords [x] = x
myUnwords xs = head xs ++ " " ++ myUnwords (tail xs)


prop_myunwords xs = myUnwords xs == unwords xs
  where types = xs :: [String]

productcon :: String -> String
productcon [x] = [x]
productcon (y:[x]) = y:[x] 
productcon (x:y:z:xs) = if y == '*' then (y:productcon xs) else (x:productcon (y:z:xs))

withspace :: String -> String
withspace [x] = [x]
withspace (x:xs) = x:(' ':withspace xs)

productex :: [String] -> [String]
productex [] = []
productex (x:xs) = if a == 1 then x:productex xs else (withspace ([w|w<-x,w `elem` "1234567890"] ++ (replicate b '*')):productex xs)
  where a = length x
        b = a `div` 2

changeorder :: [String] -> [String]
changeorder [] = []
changeorder xs = if (head (last xs)) `elem` "1234567890" then (last xs):changeorder (init xs) else (changeorder (init xs))++[last xs]

standard2RPN :: String -> String
standard2RPN xs = unwords (changeorder (productex (words (productcon xs))))
