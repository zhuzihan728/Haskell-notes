import Data.Char
inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b (x:xs) = if x>=a && x<=b then x:inRange a b xs else inRange a b xs


countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = if x > 0 then countPositives xs + 1 else countPositives xs

capitalised :: String -> String
capitalised [x] = [toUpper x]
capitalised xs = capitalised (init xs) ++ [toLower (last xs)]

lowercase :: String -> String
lowercase [] = []
lowercase xs = toLower (head xs) : lowercase (tail xs)

title :: [String] -> [String]
title [x] = [capitalised x]
title xs = title (init xs) ++ [if length (last xs) >= 4 then capitalised (last xs) else lowercase (last xs)]

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = if a <= x then a:(x:xs) else x : insert a xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if x>=y then [y] ++ merge (x:xs) ys else [x] ++ merge xs (y:ys)

msort :: Ord a => [a] -> [a]
msort [a] = [a]
msort xs =  
    let c = length xs
        a = if even c then c `div` 2 else (c+1) `div` 2
        b = c - a
    in merge (msort (take a xs)) (msort (take b (reverse xs)))

rotor :: Int -> String -> String
rotor a (x:xs) | a == 0 = x:xs
               | a == 1 = xs ++ [x] 
               | a > 1 && a < b = rotor (a-1) (xs++[x]) 
               where b = length (x:xs)

makeKey :: Int -> [(Char,Char)]
makeKey x = 
    let xs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ys = rotor x xs
    in zip xs ys

lookUp :: Char -> [(Char,Char)] -> Char
lookUp a (x:xs) | a == yi = er
                | a `elem` ['0'..'9'] = a
                | otherwise = lookUp a xs
                where yi = fst x
                      er = snd x

encipher :: Int -> Char -> Char
encipher a x = 
    let xs = makeKey a
    in lookUp x xs

normalise :: String -> String
normalise [] = []
normalise (x:xs) | x `elem` ['0'..'9'] = x:normalise xs
                 | x `elem` ['a'..'z']|| x `elem` ['A'..'Z'] = (toUpper x):normalise xs
                 | otherwise = normalise xs
    
    


encipherStr :: Int -> String -> String
encipherStr a [] = []
encipherStr a xs = let (y:ys) = normalise xs
                   in [encipher a y]++(encipherStr a ys)







        




