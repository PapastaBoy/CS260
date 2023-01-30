{-1) Using foldr define the factorial function..-}

factorial::Int -> Int
factorial = undefined

{-2)Using foldr define removeDups which remove duplicate elements from a list. -}

removeDups::Eq a => [a] -> [a]
removeDups = undefined

{-3) Below is a definition of a binary tree and instances of the Functor and Foldable typeclass.-}

data BTree a = Leaf | Node (BTree a) a (BTree a)   --Use Btree to motivate foldable typeclass
   deriving Show

instance Functor BTree where
     fmap f Leaf = Leaf 
     fmap f (Node x a y) = Node (fmap f x) (f a) (fmap f y)

instance Foldable BTree where
     foldr f x Leaf = x   
     foldr f x (Node y a z) = foldr f (f a (foldr f x y)) z

{-3a) Using these defintions define treeProd which calculate the product of values kept in a BTree-}

treeProd::Num a=> BTree a -> a 
treeProd = undefined

{-3b) Define treeSum which calculates the sum of the elements of a binary tree-}

treeSum::Num a => BTree a -> a 
treeSum = undefined

{-3c) BTrees can be used to store Strings. Define concTree which concatenates the strings stored in a BTree-}

concTree::BTree String -> String 
concTree = undefined

{-4a) One issue we might face is that lists are homogoneous, that is they can only hold one type (['a',1,True] is not valid). Define the type Hold which allows functions to work on two lists of different types simultaneously. An element of the Hold type should contain two lists of different types.-}



{-4b) Using this type define merge which merges the two lists.-}

merge::Hold a b -> [(a,b)]
merge = undefined

{-4c) Define merge2 which handles lists of different legnths by extending the shorter list using Maybe.-}

merge2:: Hold a b -> [(Maybe a, Maybe b)]
merge2 = undefined

{-5) Below is the type MyMaybe (which is just Maybe). Define a functor instance for MyMaybe-}

data MyMaybe a = MyJust a | MyNothing


    
{-6) Consider the following type which contains lists of different weight.-}

data MyType a = One a (MyType a) | Twice a (MyType a) |Thrice a (MyType a) | Empty 
 deriving Show 
 
--a) Defined myTypeFoldr which applies a right fold to MyType. Assume that a is a numeric type and the contructor represents the multiplication of the value that is held by it.

myTypeFoldr::Num a => (a -> a -> a) -> a -> MyType a -> a --simple but repetitive
myTypeFoldr = undefined

--b) define a Functor instance for the type MyType 




 
 





    

