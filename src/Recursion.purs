module Recursion where

import Prelude
import Data.Array (null, filter, (..))
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product)
import Control.MonadZero (guard)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

length :: forall a. Array a -> Int
length arr =
	if null arr 
		then 0
        else 1 + length (unsafePartial tail arr)

isEven :: Int -> Boolean 
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

evens :: Array Int -> Int
evens arr =
	if null arr
	then 0
	else 
		if isEven $ unsafePartial head arr
		then 1 + evens (unsafePartial tail arr)
		else evens (unsafePartial tail arr)

evens' :: Array Int -> Int
evens' arr = length $ do
	x <- arr
	guard $ isEven x
	pure x



square :: Array Int -> Array Int
square = map (\n -> n * n)

nonNegs :: Array Int -> Array Int
nonNegs = filter (\n -> n >= 0)

infix 8 filter as <$?>

positives :: Array Int -> Array Int
positives arr = (\n -> n >= 0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
	i <- 1 .. n
	j <- i .. n
	pure [i, j]

factors' :: Int -> Array (Array Int)
factors' n = do
	i <- 1 .. n
	j <- i .. n
	guard $ i * j == n
	pure [i, j]

--4.11.1
isPrime :: Int -> Boolean 
isPrime i = 1 == (length $ factors i)

--4.11.2
cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian a b = do
	x <- a
	y <- b
	pure [x, y]

--4.11.3
triples :: Int -> Array (Array Int)
triples n = do
	a <- 1 .. n
	b <- a .. n
	c <- b .. n 
	guard $ a * a + b * b == c * c
	pure [a, b, c]
	
--4.11.4
-- factorizations :: Int -> Array (Array Int)

