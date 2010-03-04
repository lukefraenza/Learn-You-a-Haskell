multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x


compareHundred :: (Num a, Ord a) => a -> Ordering
compareHundred = compare 100

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f = g
--   where g x y = f y x
flip' f y x = f x y

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
   where p x = x `mod` 3892 == 0

-- sum of all odd squares that are smaller than 10,000

sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Collatz sequences
-- if even, divide by 2.  if odd, multiply by 3 and add 1.
-- all starting numbers end up at 1
-- SO, for all starting numbers between 1 and 100, 
-- how many chains have a length > 15?

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd  n = n:chain (n*3 + 1)

numLongChains :: Int
--numLongChains = length ( filter isLong (map chain [1..100]))
--  where isLong xs = length xs > 15
numLongChains = length ( filter (\xs -> length xs > 15) (map chain [1..100]))
