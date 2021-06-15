insert :: Int -> [Int] -> [Int]
insert n [] = [n] 
insert y (x:xs) = if y>x then [x] ++ insert y xs else [y] ++ [a|a<-(x:xs)] 

one :: [Int] -> Int
one      [] = 0
one  (x:xs) = x+1

two :: [Int] -> Int
two [] = 0
two (x:[]) = x
two (b:(a:xs)) = a+b