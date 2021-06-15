import Data.Set
import Test.QuickCheck


data Set' a = Empty' | Node' a (Set' a) (Set' a) deriving (Show, Read)

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList' :: Set' a -> [a]
toList' Empty' = []
toList' (Node' a left right) =  (toList' left) ++ [a] ++ (toList' right)

-- fromList [2,1,1,4,5] => {2,1,4,5}

fromList' :: Ord a => [a] -> Set' a
fromList' [] = Empty'
fromList' xs = Node' a (fromList' [x|x<-xs,x<a]) (fromList' [x|x<-xs,x>a]) 
   where a = head xs

instance (Ord a) => Eq (Set' a) where
   s1 == s2 = (toList' s1) == (toList' s2)

empty' :: Set' a
empty' = Empty'

singleton' :: a -> Set' a
singleton' x = Node' x Empty' Empty'

union' :: (Ord a) => Set' a -> Set' a -> Set' a
union' s1 s2 = fromList' (xs ++ ys)
   where xs = toList' s1
         ys = toList' s2

insert' :: (Ord a) => a -> Set' a -> Set' a
insert' x Empty' = singleton' x
insert' x (Node' a left right) | x == a = Node' x left right
                               | x < a = Node' a (insert' x left) right
                               | x > a = Node' a left (insert' x right)
                           

intersection' :: (Ord a) => Set' a -> Set' a -> Set' a
intersection' set Empty' = Empty'
intersection' Empty' set = Empty'
intersection' (Node' a l1 r1) (Node' b l2 r2) | a==b = Node' a (intersection' l1 l2) (intersection' r1 r2)
                                              | a<b = union' (intersection' (Node' a l1 Empty') l2) (intersection' r1 (Node' b l2 r2))
                                              | a>b = union' (intersection' (Node' a Empty' r1) r2) (intersection' l1 (Node' b l2 r2))


difference' :: (Ord a) => Set' a -> Set' a -> Set' a
difference' Empty' set = Empty'
difference' (Node' a l1 r1) s2 = if member' a s2 then difference' (union' l1 r1) s2 else insert' a (difference' (union' l1 r1) s2)


-- is element *a* in the Set?
member' :: (Ord a) => a -> Set' a -> Bool
member' x Empty' = False
member' x (Node' a left right) | x == a = True  
                               | x < a  = member' x left  
                               | x > a  = member' x right 


cardinality' :: Set' a -> Int
cardinality' set = length (toList' set)


setmap' :: (Ord b) => (a -> b) -> Set' a -> Set' b
setmap' f set = fromList' (Prelude.map f xs)
   where xs = toList' set

setfoldr' :: (a -> b -> b) -> Set' a -> b -> b
setfoldr' f set z = Prelude.foldr f z xs
   where xs = toList' set

prop_a a xs ys = ((toList $ singleton a) == (toList' $ singleton' a)) && (toList (union s1 s2) == toList' (union' s3 s4)) && (toList (Data.Set.insert a s1) == toList' (insert' a s3)) && ((toList $ intersection s1 s2) == (toList' $ intersection' s3 s4))&&((toList $ difference s1 s2)==(toList' $ difference' s3 s4)) &&((member a s1) == (member' a s3))&&(cardinality s1 == cardinality s3)
   where types = (a::Int,xs::[Int],ys::[Int])
         s1 = fromList xs
         s2 = fromList ys
         s3 = fromList' xs
         s4 = fromList' ys