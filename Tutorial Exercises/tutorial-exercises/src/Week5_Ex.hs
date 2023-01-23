module Week5_Ex () where
{-Today we will look at polymorphism, typeclasses and higher order functions..  -}

--1) Define a function of type a -> a 

fun:: a -> a
fun a = a

--2) The following maxVal function has an issue in that it assumes the largest value must be larger than zero. maxVal uses the maxHelper to carry out the heavy lifting

maxVal::(Bounded a, Ord a) => [a] -> a
maxVal = maxHelper minBound

maxHelper::(Bounded a, Ord a) => a -> [a] -> a
maxHelper m [] = m                  
maxHelper m (x:xs) | m >= x = maxHelper m xs
                   | otherwise = maxHelper x xs

--Using the typeclass Bounded and any other typeclass you may need improve maxHelper and maxVal. Your final solution should not assume the largest value is greater than 0 and should have the most generic type possible. 

--3) Some typeclasses subsume the functionality of others. That is, some classes contain enough functionality to define functionality from other clases. Using only functionality from the Ord typeclass define a function equals whch for two values returns True when they are equivelant and false otherwise. You may not use (==).

equals::Ord a => a -> a -> Bool 
equals x y | x < y || x > y = False
           | otherwise = True

--4) Define higher checksum which checks, for a function f and three inputs a,b and c if f c = f a + f b

higherCheckSum::(Eq a,Num a) => (a -> a) -> a -> a -> a -> Bool 
higherCheckSum f a b c | f a + f b == f c = True
                       | otherwise = False

--5) What is the type of map map? 

--6) Define takeWhile' which takes elements from a list while a boolean condition is met. When the condition is not met takewhile should stop returning the values up till that point, for example takeWhile' even [2,4,6,1,3,5,6] returns [2,4,6]

takeWhile'::(a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x:takeWhile' f xs
                    | otherwise = []

--7) Define dropWhile' which drops values while a given boolean condition is met, dropWhile' even [2,4,6,1,3,5,6] = [1,3,5,6]

dropWhile'::(a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs

--8) Using takeWhile' and dropWhile' define segments which splits a list on elements which return false for a given boolean function. Hint, the drop function may be useful. segments odd [1,1,1,2,3,1,4,12,3,4,5,5] = [[1,1,1],[3,1],[],[3],[5,5]]. 

segments :: (a -> Bool) -> [a] -> [[a]]
segments _ [] = [[]]
segments f (x:xs) | f x =  [takeWhile' f (x:xs)] ++ segments f (dropWhile' f xs)
                  | otherwise = [takeWhile' f xs] ++ segments f (dropWhile' f xs)


--9) Using filter define a function returnList which returns the same list as it was given.

returnList::[a] -> [a]
returnList = filter (const True) 

--10) Define numRange which takes a function as well as two numbers. numRange should then apply the function to the range of values generated from the two values supplied. You may assume that the first integer
-- is smaller than the second. 


numRange::(Int -> Int) -> Int -> Int -> [Int]
numRange f x y = map f [x..y] 

