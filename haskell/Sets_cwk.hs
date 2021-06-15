module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type,
  wrap a binary search tree, or even use a self-balancing binary search tree.
  Extra marks will be awarded for efficient implementations (a self-balancing tree will be
  more efficient than a linked list for example).

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
data Set a = Empty | Node a (Set a) (Set a) deriving (Ord)

showSet :: (Show a) => Set a -> String
showSet Empty = ""
showSet (Node a left right) = showSet left ++ "," ++ show a ++ showSet right

instance (Show a) => Show (Set a) where
   show Empty = "{}"
   show a = "{" ++ (tail $ showSet a) ++ "}"
   


{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList Empty = []
toList (Node a left right) =  (toList left) ++ [a] ++ (toList right)

-- fromList [2,1,1,4,5] => {2,1,4,5}

fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = Node x (fromList [a|a<-xs,a<x]) (fromList [a|a<-xs,a>x]) 


{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1) == (toList s2)


-- the empty set
empty :: Set a
empty = Empty


-- Set with one element
singleton :: a -> Set a
singleton x = Node x Empty Empty


-- insert an element of type a into a Set
-- make sure there are no duplicates!

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = singleton x
insert x (Node a left right) | x == a = Node x left right
                             | x < a = Node a (insert x left) right
                             | x > a = Node a left (insert x right)


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = Prelude.foldr insert s1 (toList s2)


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection set Empty = Empty
intersection Empty set = Empty
intersection (Node a l1 r1) s2 = if member a s2 then insert a (intersection (union l1 r1) s2) else intersection (union l1 r1) s2

-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}

difference :: (Ord a) => Set a -> Set a -> Set a
difference Empty set = Empty
difference (Node a l1 r1) s2 = if member a s2 then difference (union l1 r1) s2 else insert a (difference (union l1 r1) s2)


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Node a left right) | x == a = True  
                             | x < a  = member x left  
                             | x > a  = member x right  


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality set = length (toList set)


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f set = fromList (Prelude.map f xs)
   where xs = toList set

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f set z = Prelude.foldr f z xs
   where xs = toList set


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }

addlist :: [[a]]->[a]->[[a]]
addlist xs [] = xs
addlist xs (y:ys) = addlist ([] : [y:zs|zs<-xs] ++(tail xs)) ys

subset :: [a] -> [[a]]
subset xs = addlist [[]] (reverse xs)

list2set :: [a] -> Set a
list2set [] = Empty
list2set (x:xs) = Node x Empty (list2set xs)

powerSet :: Set a -> Set (Set a)
powerSet set = list2set (Prelude.map list2set (subset xs))
   where xs = toList set

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian s1 s2 = list2set [(a,b)|a<-xs,b<-ys]
   where xs = toList s1
         ys = toList s2


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
antifilter :: (a -> Bool) -> [a] -> [a]
antifilter p [] = []
antifilter p (x:y) | p x = antifilter p y
                   | otherwise = x:antifilter p y

setantifilter :: (a -> Bool) -> Set a -> Set a
setantifilter f set = list2set (antifilter f xs)
   where xs = toList set

setfilter :: (a -> Bool) -> Set a -> Set a
setfilter f set = list2set (filter f xs)
   where xs = toList set
   
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f set = (s1,s2)
   where s1 = setfilter f set
         s2 = setantifilter f set


{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

   The maximum mark for those who use lists, as in the example above, is 70%. To achieve
   a higher grade than is, one must write a more efficient implementation.
   100% is reserved for those brave few who write their own self-balancing binary tree.
-}
