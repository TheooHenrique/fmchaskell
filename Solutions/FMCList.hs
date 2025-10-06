{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (a : as) = a

tail :: [a] -> [a]
tail [] = undefined
tail (a : as) = as

null :: [a] -> Bool
null [] = True
null (a : as) = False

length :: Integral i => [a] -> i
length [] = 0
length (a : as) = length as + 1

sum :: Num a => [a] -> a
sum  [] = 0
sum (n : ns) = sum ns + n

product :: Num a => [a] -> a
product [] = 0
product (a : []) = a
product (a : as) = product as * a

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ n = n
(y : ys) ++ n = y : (ys ++ n)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc c [] = [c]
snoc y a = a ++ [y]

(<:) :: [a] -> a -> [a]
[] <: c = [c]
a <: c = a ++ [c]


-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = undefined
minimum [x] = x
minimum [x, y] = if x < y then x else y
minimum (x : xs) = if x < minimum xs then x else minimum xs

maximum :: Ord a => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum [x, y] = if x > y then x else y
maximum (x : xs) = if x > maximum xs then x else maximum xs

-- take
take :: Integral i => i -> [a] -> [a]
take 0 l = []
take n (x : xs) = if n == length (x : xs) then x : xs else take n (init (x : xs))

-- drop
drop 0 l = l
drop n (x : xs) = if length (x : xs) == n then [] else drop (n-1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x : xs) = if f x then x : takeWhile f xs else []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x : xs) = if f x then dropWhile f xs else x : xs

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails [a] = [[a], []]
tails (x : xs) = (x : xs) : tails xs

-- init
init :: [a] -> [a]
init [] = []
init [a] = [a]
init (x : xs) = reverse (tail (reverse (x : xs)))

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits [a] = [[], [a]]
inits (x : xs) = inits (init (x : xs)) ++ [x : xs]

-- subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = subsequences xs ++ map (x :) (subsequences xs)

-- any
any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x : xs) = f x || any f xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x : xs) = f x && all f xs

-- aux: istrue
istrue :: Bool -> Bool
istrue True = True
istrue False = False

-- and
and :: [Bool] -> Bool
and [] = True
and (x : xs) = all istrue (x : xs)

-- or
or :: [Bool] -> Bool
or [] = False
or (x : xs) = any istrue (x : xs)

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y = any (==y)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' w (x : xs) = w == x || elem' w xs

-- (!!)
(!!) :: [a] -> Int -> a
(x : xs) !! n = if length (x : xs) >= n + 1 then head (reverse (take (n + 1) (x : xs))) else undefined

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

-- map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = undefined
cycle l = l ++ cycle l

-- repeat
repeat :: a -> [a]
repeat a = a : repeat a

-- replicate
replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = a : replicate (n-1) a

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] l = True
isPrefixOf xs ys = elem xs (inits ys)

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = elem xs (subsequences ys)

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] l = True
isSuffixOf xs ys = elem xs (tails ys)

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip a [] = []
zip [] b = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f a [] = []
zipWith f [] b = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate l [] = []
intercalate l [[a]] = [a]
intercalate l (x : xs) = x ++ l ++ intercalate l xs

-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub [a] = [a]
nub (x : xs) = if elem x xs then nub xs else x : nub xs

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 l = ([], l)
splitAt n [] = ([], [])  --The problem was here! The mentioned implementation did not encompass this base.
splitAt n l = (take n l, drop n l)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break f [] = ([], [])
break f (x : xs) = (takeWhile (not . f) (x : xs) , dropWhile (not . f) ( x : xs))

-- lines
lines :: String -> [String]
lines [] = []
lines [a] = [[a]]
lines (x : xs) = undefined

-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome [] = True
palindrome l = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

