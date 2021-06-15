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
newtype Set a = Set { unSet :: [a] } deriving (Show)




{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
qsort :: Ord a => [a] -> [a] 
qsort [] = []
qsort (x:xs) = qsort[a|a <- xs,a <= x] ++ [x] ++ qsort[b|b<- xs,b > x]

toList :: Set a -> [a]
toList a = unSet a

-- fromList [2,1,1,4,5] => {2,1,4,5}
norep :: Ord a => [a] -> [a]
norep [] = []
norep xs = if head xs `elem` tail xs then norep (tail xs) else (head xs):norep (tail xs)

fromList :: Ord a => [a] -> Set a
fromList xs = Set (norep xs)


{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = qsort (unSet s1) == qsort (unSet s2)


-- the empty set
empty :: Set a
empty = Set []


-- Set with one element
singleton :: a -> Set a
singleton a = Set [a]


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert a set = if a `elem` (unSet set) then set else Set (a:(unSet set))


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = fromList ((unSet s1)++(unSet s2)) 


-- return the common elements between two Sets
interlist :: (Ord a) => [a] -> [a] -> [a]
interlist [] ys = []
interlist xs [] = []
interlist (x:xs) ys = if x `elem` ys then x:(interlist xs ys) else interlist xs ys

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = Set (interlist (unSet s1) (unSet s2))


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = Set ([x|x<-xs,not(x `elem` ys)])
   where xs = (unSet s1)
         ys = (unSet (intersection s1 s2))


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member a set = a `elem` (unSet set)


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality set = length (unSet set) 


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f set = Set (map f xs)
   where xs = unSet set 



setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f set z = foldr f z xs
   where xs = unSet set


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }

addlist :: [[a]]->[a]->[[a]]
addlist xs [] = xs
addlist xs (y:ys) = addlist ((map (y:) xs)++xs) ys

powerSet :: Set a -> Set (Set a)
powerSet set = Set (map Set (addlist [[]] xs))
   where xs = unSet set


-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian s1 s2 = Set [(a,b)|a<-xs,b<-ys]
   where xs = unSet s1
         ys = unSet s2


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right

antifilter :: (a -> Bool) -> [a] -> [a]
antifilter p [] = []
antifilter p (x:y) | p x = antifilter p y
                   | otherwise = x:antifilter p y

setantifilter :: (a -> Bool) -> Set a -> Set a
setantifilter f set = Set (antifilter f xs)
   where xs = unSet set

setfilter :: (a -> Bool) -> Set a -> Set a
setfilter f set = Set (filter f xs)
   where xs = unSet set
   
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
