module Week2 () where

{-For these exercises it is recommended that you use Stack to run GHCI. Once you have downloaded and saved this file you should open a terminal and navigate to the correct folder. Once you are in the correct folder run ghci using Stack, you will then be able to load this file using :l Ex1.hs . Define the required functions in the file using the already provided names and types! Do not change the naming or typing of functions. When defining a function f replace the line f = undefined with your definition. -}

-- 1) Define the following:
--(a) a checkSum function that takes input a, b and c evaluating to true when a + b = c

checkSum :: Int -> Int -> Int -> Bool
checkSum a b c | a + b == c = True
               | otherwise  = False

--(b) Define a function notdivisible::Int -> Int -> Bool that evaluates to True on input a and b if and only if a is not divisible by b.

notDivisible :: Int -> Int -> Bool
notDivisible a b | a `mod` b == 0 = True
                 | otherwise      = False

--2) Define evenFactorial which, for a value n, multiplies all of the even numbers less than or equal to n. For example evenFactorial 6 should return 6*4*2.

evenFactorial :: Int -> Int
evenFactorial x | x < 0 = error "Factorial negative given"
evenFactorial x | x < 2 = 1
                | x `mod` 2 == 1 = evenFactorial (x - 1)
                | otherwise = x * evenFactorial (x - 2)
                
evenFactorialFold :: Int -> Int
evenFactorialFold n = product [2,4..n]

{-3)
  a)The Collatz function is defined as follows:
f(x) = { x/2,    if x is even
       { 3x + 1, if x is odd
Define this function in Haskell (you may want to use div instead of /). -}

collatz :: Int -> Int
collatz x | x < 1 = error "Collatz Negative or 0"
collatz 1 = 1
collatz x | even x = x `div` 2
          | otherwise = 3 * x + 1

--b) The collatz conjecture states that for any integer n repeated application of the collatz function will eventually reduce n to 1. Write a function which will count the number of applications needed to return a value of 1. The first argument should count the number of applications while the second holds the current value of n

collatzApp :: Int -> Int -> Int
collatzApp c 1 = c
collatzApp c n = collatzApp (c + 1) $ collatz n


--Note. The collatz conjecture has yet to be proven, despite its simple appearance. Here is some code that will return the number of needed applications for every number from 1 to 1000.

testCollatz = map (collatzApp 0) [1..1000]

{-4) The factorial can be defined as follows:

factorial::Int -> Int
factorial 0 = 1
factorial n = n * factorial n-1

Note that this version will recurse infinitely on a negative number. Change this version to include a case which manages negative values.

-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial x | x < 0 = x * factorial (x + 1) 
            | otherwise = x * factorial (x - 1)    
            
fibs :: [Int] 
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Int -> Int
fibonacci n = fibs !! n

--5) Defined the combinatorial function for numbers n and m given by n!/(m!*(n-m)!), this should return an error if n < m

comb :: Integer -> Integer -> Integer
comb n m | n < m = error "n < m"
comb n m = factorial n `div` factorial m * factorial (n - m)

--6) Define leapYear which returns a String telling the user whether a supplied year is a leap yer.

leapYear :: Int -> String
leapYear n | n < 0 = error "year cannot be negative"
           | n `mod` 100 == 0 && n `mod` 400 /= 0 = "Not Leap Year"
           | n `mod` 4 == 0 = "Leap Year"
           | otherwise = "Not Leap Year"

--7) Define the function prime which returns True if a given number n is prime. Remember that a number is prime if it is not divisible by any number other than 1 and itself. It may help to define a helper function

        
prime::Integer -> Bool
prime n = [x | x <- [1..n], mod n x == 0] == [1, n]

testPrime = map prime [1..1000]