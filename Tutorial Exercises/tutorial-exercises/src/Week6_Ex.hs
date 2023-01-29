module Week6_Ex () where


{-Types-}

--1)   Using the following definition of Nat, plus and mult define
--i)   expo, the exponential of a given natural number to the power of another given number
--ii)  The factorial for Nat
--iii) absNat which converts an integer value to the absolute value expressed by Nat

data Nat = Succ Nat | Zero
   deriving Show

myadd:: Nat -> Nat -> Nat
myadd Zero x = x
myadd (Succ x) y = Succ (myadd x y)

mymult :: Nat -> Nat -> Nat
mymult Zero n = Zero
mymult (Succ m) n = myadd n (mymult m n)


expo::Nat -> Nat -> Nat 
expo Zero x = Zero
expo x Zero = Succ Zero
expo x (Succ y) = x `mymult` expo x y

fact::Nat -> Nat
fact Zero = Succ Zero
fact (Succ x) = Succ x `mymult` fact x

absNat::Int -> Nat
absNat 0 = Zero
absNat x = Succ $ absNat (x-1)

natAbs::Nat -> Integer
natAbs Zero = 0
natAbs (Succ x) = 1 + natAbs x


--2a) Define a type Day an element of which is one of the days Monday - Sunday. Equip this type with an instance of Eq, Show and Ord.

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
   deriving (Eq, Show, Ord, Enum)

--2b) For the day type define a function workDay which returns True if a day is a workday.

workDay::Day -> Bool
workDay d | d < Saturday = otherwise
          | otherwise = not otherwise


--2c) Define nextDay which, for an element of the Day type returns the next day.

--nextDay::Day -> Day
nextDay Sunday = Monday
nextDay d = succ d

--3) Consider the following type Calc. This type is parametarised by an abstratct type a but is intended for use with numeric types. 
data Calc a = Add (Calc a) (Calc a) | Mult (Calc a) (Calc a) | Div (Calc a) (Calc a) | Sub (Calc a) (Calc a) | Val a
   deriving Show

--3a) Using this type define apply which takes a value of type Calc Int and returns the Int resulting from the calculation
-- Khosrowhshahhi

apply::Calc Int -> Int
apply (Add a b) = apply a + apply b
apply (Sub a b) = apply a - apply b
apply (Mult a b) = apply a * apply b
apply (Div a b) = apply a `div` apply b
apply (Val a) = a
  
--3b) Define factorial:: Calc Int -> Calc Int which takes a value of the shape (Val n) returning the factorial of that value. You must use the Calc type (Val (factorial n) doesn't count!).

factorial::Calc Int -> Calc Int
factorial (Val 0) = Val 1
factorial (Val n) = Val (apply (Mult (Val n) (factorial (Val (apply (Sub (Val n) (Val 1)))))))

--3c) The Calc type can be seen as a functor, given an instantiation of the Functor class for Calc

instance Functor Calc where
  fmap :: (a -> b) -> Calc a -> Calc b
  fmap f (Val a) = Val (f a)
  fmap f (Add a b) = Add (fmap f a) (fmap f b)
  fmap f (Mult a b) = Mult (fmap f a) (fmap f b)  
  fmap f (Sub a b) = Sub (fmap f a) (fmap f b)  
  fmap f (Div a b) = Div (fmap f a) (fmap f b)  
  
--4a) Define the type MkCom which is paramterarised by an abstract type. MkCom represents a list which may contain null values. The type should contain Elem which holds an element of type a, Null which is an empty value and Empty which is the emd of the list.

data MkCom a = Elem a (MkCom a) | Empty | Null (MkCom a)
   deriving Show
   
--4b) Define flatten which removes all null elements from a list of type MkCom

flatten:: MkCom a -> MkCom a
flatten Empty = Empty
flatten (Null as) = flatten as
flatten (Elem a as) = Elem a (flatten as)

--4c) Define convert which converts a list of type MkCom a to [a], you may discard null values.

convert::MkCom a -> [a]
convert Empty = []
convert (Null a) = convert a
convert (Elem a as) = a : convert as

 