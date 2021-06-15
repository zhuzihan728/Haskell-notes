mult :: (Num a) => [a] -> a
mult xs = foldr (\acc x -> acc * x) 1 xs

posList :: [Int] -> [Int]
posList xs = filter (>0) xs

trueList :: [Bool] -> Bool
trueList xs = foldr (\acc x -> acc && x) True xs

evenList :: [Int] -> Bool
evenList xs = trueList (map even xs)

maxList :: (Ord a) => [a] -> a
maxList xs = foldr (\acc x -> if acc > x then acc else x) (head xs) xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = filter (<=b) (filter (>=a) xs)

countPositives :: [Int] -> Int
countPositives xs = foldr (\x acc -> if x>0 then succ acc else acc) 0 xs

myLength :: [a] -> Int
myLength xs = foldr (\x acc -> acc+1) 0 xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> (f x):acc) [] xs
