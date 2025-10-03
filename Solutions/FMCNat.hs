{-# LANGUAGE GADTs #-}

module FMCNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where  --DÁ PRA MELHORAR MAS DEPOIS EU PENSO NISSO
    O == O = True
    S n == O = False
    O == S n = False
    S n == S m = n == m

instance Ord Nat where

    O <= n = True
    n <= O = False
    S n <= S m = max (S m) (S n) == S m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O n = O
    min n O = O
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)



----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S O) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O <+> n = n
(S n) <+> m = S (n + m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O = n
monus (S n) m = if S n <= m then O else S (monus n m)

(-*) :: Nat -> Nat -> Nat
n -* O = n
(S n) -* m = if S n <= m then O else S ((-*) n m)

-- multiplication
times :: Nat -> Nat -> Nat
times O n = O
times n O = O
times (S n) m = times n m + m

(<*>) :: Nat -> Nat -> Nat
O <*> _ = O
_ <*> O = O
(S n) <*> m = (<*>) n m + m

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow (S _) O = S O
pow O (S _) = O
pow (S n) (S m) = pow (S n) m * S n


exp :: Nat -> Nat -> Nat
exp (S n) O = S O
exp O (S n) = O
exp (S n) (S m) = exp (S n) m * S n

(<^>) :: Nat -> Nat -> Nat
S n <^> O = S O
O <^> (S n) = O
(S n) <^> (S m) = (<^>) (S n) m * S n

-- quotient
(</>) :: Nat -> Nat -> Nat
O </> S n = O
n </> S O = n
(</>) (S n) (S m) = n --isso ta quebrado


-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = undefined

(|-|) = dist

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    --(-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

