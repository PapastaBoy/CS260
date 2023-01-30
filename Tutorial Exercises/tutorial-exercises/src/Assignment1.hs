module Assignment1 () where

{-Assignment 1
-------------------------------------------
Submit you answers using the myplace link as  <Your name>.hs file. For all questions you may not change the type of the function, you may define helper functions for any question. You may use any functions within the Haskell prelude unless otherwise specified. Solutions must be submitted via the myplace link by 18/02/22 17:00. Partial solutions may still attract marks  -}

  
{-1) Define buildSquare which builds the list of numbers whose square is less than a given value. e.g. buildSquare 50 = [1,2,3,4,5,6,7] , buildSquare 10 = [1,2,3] ( 2 marks)
-}

buildSquare:: Int -> [Int]
buildSquare n = [x | x <- [1..n], x ^ 2 < n] 

{-2) Define longerList which takes 2 lists and returns True if the first is longer than the second and False otherwise. (2 marks)-}

longerList::[a] -> [a] -> Bool
longerList a b = length a > length b

{-3)a) Write a function insert that takes as its first argument a function, as second argument an element of a and as
third argument a list of type a and inserts x just before the first element y of xs such that f y > f x (if such a y
does not exist, x should be inserted at the end of the list).(2 marks)-}

insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert f x [y] | f y > f x = [x,y]
               | otherwise = [y,x]
insert f x (y:xs) | f y > f x = x:y:xs
                  | otherwise = y : insert f x xs

{-b) Use insert to define a function inssort that sorts a given list such that the sorted list satisfies the following
condition: x occurs before y implies f x <= f y- (3 marks)-}

inssort :: Ord b => (a -> b) -> [a] -> [a]
inssort _ [] = []
inssort f (x:xs) = insert f x (inssort f xs)

----------------------------------

data Bit = O | I
 deriving (Show,Eq) 

{-Here the Bit type has been declared as either an I value or a O value. We will use this type in some of the following questions, you can view it as being extremely similar to the Bool type. Treat I as the
 bit value 1 and O as 0.  -}

{-4)a) Define your own function myAnd which applies logical and to two given Bit values. e.g myAnd I I = I, myAnd O I = O. (1 mark)-}

myAnd::Bit ->  Bit -> Bit
myAnd I I = I
myAnd _ _ = O

{-b) We can use lists of bits to represent binary numbers. Using myAnd define bitwiseAnd which takes two lists of Bits and applies myAnd to the corresponding elements of each list. Your bitewiseAnd should
manage lists of different lengths by discarding excess values.  (2 marks) -}

bitwiseAnd::[Bit] -> [Bit] -> [Bit]
bitwiseAnd = zipWith myAnd



{-c) Define bit2Int which converts a binary number, represented as a list of bits, to an integer. You must consider the place value of each Bit with the first element being the most significant and the last
 being the least significant. (3 marks)
    bit2Int [O,I] = 1
    bit2Int [I,O,I] = 5 
    bit2Int [I,I,I] = 7 
    bit2Int [I,O,O,O] = 8-}

convert::Bit -> Int
convert O = 0
convert I = 1


bit2Int:: [Bit] -> Int
bit2Int xs = foldr (\x y -> convert x + y * 2) 0 (reverse xs)

-------------------------------------

{-5) Define the remNth function which removes every nth value from a given list. e.g remNth 2 "asasasasas" = "aaaaa" and remNth 1 [1,2,3,4,5] = []. For a value of 0 your function should return the original list. (2 marks)-}


remNth::Int -> [a] -> [a]
remNth 0 xs = xs
remNth n xs = map fst (filter (\(_,x) -> x `mod` n /= 0) (zip xs [1..(length xs)]))


{-6) Consider the following algorithm for reversing the digits of an integer.

 fun(x) {
     y = 0;
    while(x>0){
        z = x `mod` 10;
        y = y*10 +z;
        x = x `div` 10;
    }
    return y;
 }
 implement a recursive version of this algorithm. Note that ending zeros will be dropped (1230 -> 321), this is fine. (3 marks)-}

revDig::Int-> Int
revDig = undefined 


testDivHel x = revDig x == (read.reverse.show) x --call this to test your revDig, it should always return true if your definition is correct. 






