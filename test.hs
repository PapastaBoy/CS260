mult5 :: Int -> Int
mult5 x = x * 5

factorial :: Int -> Int
factorial 0 = 1
factorial x | (x < 0) = error "Negative"
            | otherwise = x * factorial (x - 1)

evenStevens y = [x | x <- [1..], even x, x `mod` y == 0]

countS :: String -> Char -> Int
countS [] c = 0
countS (x:xs) c | x == c  = 1 + countS xs c
                | otherwise = countS xs c

secondLast :: [Int] -> Int
secondLast [] = error "Empty List Given"
secondLast (x:xs) | xs == [] = x
                  | length xs == 1 = x
                  | otherwise = secondLast xs

reversse :: String -> String
reversse [] = ""
reversse xs | length xs == 1 = xs
reversse (x:xs) | xs == ""  = [x]
                | otherwise = reversse xs ++ [x]

palindrome :: String -> Bool
palindrome [] = True
palindrome xs | length xs == 1 = True
palindrome xs = xs == reversse xs

flatten :: [[Int]] -> [Int]
flatten ((x:xs):ys) | ys == [] && xs == [] = [x]
                    | xs == [] = [x] ++ flatten (ys)
                    | otherwise = [x] ++ flatten ([xs] ++ ys)