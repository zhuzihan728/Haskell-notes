import Data.Set
import Test.QuickCheck
import Data.Char

data Set' a = Empty | Node a (Set' a) (Set' a) deriving (Ord)

showSet :: (Show a) => Set' a -> String
showSet Empty = ""
showSet (Node a left right) = showSet left ++ "," ++ show a ++ showSet right

instance (Show a) => Show (Set' a) where
   show Empty = "{}"
   show a = "{" ++ (tail $ showSet a) ++ "}"
   


{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList' :: Set' a -> [a]
toList' Empty = []
toList' (Node a left right) =  (toList' left) ++ [a] ++ (toList' right)

-- fromList [2,1,1,4,5] => {2,1,4,5}

fromList' :: Ord a => [a] -> Set' a
fromList' [] = Empty
fromList' (x:xs) = Node x (fromList' [a|a<-xs,a<x]) (fromList' [a|a<-xs,a>x])


{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set' a) where
  s1 == s2 = (toList' s1) == (toList' s2)


-- the empty set
empty' :: Set' a
empty' = Empty


-- Set with one element
singleton' :: a -> Set' a
singleton' x = Node x Empty Empty


-- insert an element of type a into a Set
-- make sure there are no duplicates!

insert' :: (Ord a) => a -> Set' a -> Set' a
insert' x Empty = singleton' x
insert' x (Node a left right) | x == a = Node x left right
                              | x < a = Node a (insert' x left) right
                              | x > a = Node a left (insert' x right)


-- join two Sets together
-- be careful not to introduce duplicates.
union' :: (Ord a) => Set' a -> Set' a -> Set' a
union' s1 s2 = Prelude.foldr insert' s1 (toList' s2)


-- return the common elements between two Sets
intersection' :: (Ord a) => Set' a -> Set' a -> Set' a
intersection' set Empty = Empty
intersection' Empty set = Empty
intersection' (Node a l1 r1) s2 = if member' a s2 then insert' a (intersection' (union' l1 r1) s2) else intersection' (union' l1 r1) s2

-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}

difference' :: (Ord a) => Set' a -> Set' a -> Set' a
difference' Empty set = Empty
difference' (Node a l1 r1) s2 = if member' a s2 then difference' (union' l1 r1) s2 else insert' a (difference' (union' l1 r1) s2)


-- is element *a* in the Set?
member' :: (Ord a) => a -> Set' a -> Bool
member' x Empty = False
member' x (Node a left right) | x == a = True  
                              | x < a  = member' x left  
                              | x > a  = member' x right  


-- how many elements are there in the Set?
cardinality' :: Set' a -> Int
cardinality' set = length (toList' set)


setmap' :: (Ord b) => (a -> b) -> Set' a -> Set' b
setmap' f set = fromList' (Prelude.map f xs)
   where xs = toList' set

setfoldr' :: (a -> b -> b) -> Set' a -> b -> b
setfoldr' f set z = Prelude.foldr f z xs
   where xs = toList' set


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }

addlist :: [[a]]->[a]->[[a]]
addlist xs [] = xs
addlist xs (y:ys) = addlist ([] : [y:zs|zs<-xs] ++(tail xs)) ys

subset :: [a] -> [[a]]
subset xs = addlist [[]] (reverse xs)

list2set :: [a] -> Set' a
list2set [] = Empty
list2set (x:xs) = Node x Empty (list2set xs)

powerSet' :: Set' a -> Set' (Set' a)
powerSet' set = list2set (Prelude.map list2set (subset xs))
   where xs = toList' set

-- cartesian product of two sets
cartesian' :: Set' a -> Set' b -> Set' (a, b)
cartesian' s1 s2 = list2set [(a,b)|a<-xs,b<-ys]
   where xs = toList' s1
         ys = toList' s2


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
antifilter :: (a -> Bool) -> [a] -> [a]
antifilter p [] = []
antifilter p (x:y) | p x = antifilter p y
                   | otherwise = x:antifilter p y

setantifilter :: (a -> Bool) -> Set' a -> Set' a
setantifilter f set = list2set (antifilter f xs)
   where xs = toList' set

setfilter :: (a -> Bool) -> Set' a -> Set' a
setfilter f set = list2set (Prelude.filter f xs)
   where xs = toList' set
   
partition' :: (a -> Bool) -> Set' a -> (Set' a, Set' a)
partition' f set = (s1,s2)
   where s1 = setfilter f set
         s2 = setantifilter f set


totuple :: (Ord a) => (Set' a, Set' a) -> (Set a, Set a)
totuple (s1, s2) = (fromList $ toList' s1, fromList $ toList' s2)

prop_a a xs ys = (toList s1 == toList' s3)&&(toList s2 == toList' s4)&&((toList $ singleton a) == (toList' $ singleton' a)) && (toList (union s1 s2) == toList' (union' s3 s4)) && (toList (Data.Set.insert a s1) == toList' (insert' a s3)) && ((toList $ intersection s1 s2) == (toList' $ intersection' s3 s4))&&((toList $ difference s1 s2)==(toList' $ difference' s3 s4)) &&((member a s1) == (member' a s3))&&((length s1) == (cardinality' s3))&&((Data.Set.foldr (\acc x -> if x==acc then acc else x) a s1)==(setfoldr' (\acc x -> if x==acc then acc else x) s3 a))&&((toList $ Data.Set.map (==a) s1)==(toList' $ setmap' (==a) s3)) && ((partition (>a) s1) == totuple (partition' (>a) s3)) 
   where types = (a :: Char, xs::[Char], ys::[Char])
         s1 = fromList xs
         s2 = fromList ys
         s3 = fromList' xs
         s4 = fromList' ys

         


prop_c xs = toList' (setmap' toList' (powerSet' s3)) == toList (Data.Set.map toList (powerSet s1))
   where types = xs::[Int]
         ys = Prelude.take 10 xs   
         s1 = fromList ys
         s3 = fromList' ys
