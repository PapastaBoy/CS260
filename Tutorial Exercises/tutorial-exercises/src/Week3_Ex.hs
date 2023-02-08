module Week3_Ex () where

--1) Defined the function addOne which adds 1 to each element in a list of integers.

addOne::[Int] -> [Int]
addOne = map (+ 1)

--How many times will addOne be called on the list [1,2,3,4,5]?

--2) Define a function cycleinc::[Integer]->[Integer] that creates an infinite list by repeating a given finite list indefinitely, adding in each repetition 1 to each element of the original list.
-- Example:    cycleinc [1,2,3] = [1,2,3,2,3,4,3,4,5..]

cycleInc::[Int] -> [Int]
cycleInc [] = []
cycleInc xs = xs ++ cycleInc (addOne xs)

--3) Write a function pythTriples which, for an integer n, returns all triples such that a^2 + b^2 = c^2 and  a,b,c <= n

pythTriples :: Int -> [(Int,Int,Int)]
pythTriples n = [(a,b,c) | c <- [2..n], b <- [2..n], a <- [2..n], a*a + b*b == c*c]


--4) Define a function threeStrikes which returns "You're Out!" if it finds three occurences of a given value in a list returning "all good" otherwise. Hint. you may want to define a helper function, also consider whether or not your function would work with infinite lists.

counter :: Char -> String -> Int
counter _ [] = 0
counter c (x:xs) | c == x = 1 + counter c xs
                 | otherwise = counter c xs

threeStrikes::Char -> String -> String 
threeStrikes c xs | c == '\0' = error "cannot use an empty character"
                  | xs == "" = error "cannot use empty string"
                  | counter c xs >= 3 = "You're Out!"
                  | otherwise = "all good"                  

--5) Define the function sumEven which sums the even values of a list

sumEven::[Int] -> Int
sumEven [] = 0
sumEven (x:xs) | even x = x + sumEven xs
               | otherwise = sumEven xs

--6) Define a function occursTwice wich returns True if a character occurs twice in a row in a given string and False otherwise.

occursTwice::Char -> String -> Bool 
occursTwice c _ | c == '\0' = error "cannot evaluate empty character"
occursTwice c [y] = False
occursTwice c (x:y:xs) | c == x && x == y = True
                       | otherwise = occursTwice c (y:xs)

--lift occursTwice to work on lists of strings.

otList::Char -> [String] -> [Bool]
otList _ [] = []
otList _ (x:_) | null x = [False]
otList c (x:xs) = occursTwice c x : otList c xs

--lift otList to work on lists of lists!

otListList :: Char -> [[String]] -> [[Bool]]
otListList _ [] = []
otListList _ [[]] = [[]]
otListList c (xs:ys) = otList c xs : otListList c ys

--7) Without using the maximum function define maxList which finds the maximum value in a list of integers.

maxList :: [Int] -> Int 
maxList [] = minBound
maxList (x:xs) | x > maxList xs = x
               | otherwise = maxList xs

--Use this function to define sortList which sorts a list from smallest to largest.

deleteMax::[Int] -> [Int]
deleteMax [x] = []
deleteMax (x:xs) | x == maxList (x:xs) = xs
                 | otherwise = x:deleteMax xs

sortList::[Int] -> [Int]
sortList [] = []
sortList [x] = [x]
sortList xs = sortList (deleteMax xs) ++ [maxList xs]


--8) Define findPos which returns the character at a given position in a string

findPos :: Int -> String -> Char 
findPos x xs | x < 0 = error "cannot find negative index"
            | x > (length xs - 1) = error "index out of bounds"
findPos x (c:cs)| x == 0 = c
                | otherwise = findPos (x-1) cs

--9) Define rev2 which reverses lists of length 2  and leaves all other lists

rev2 ::[Int] -> [Int]
rev2 xs | length xs < 2 = xs
rev2 (x:y:xs) | null xs = y:[x]
              | otherwise = x:y:xs
