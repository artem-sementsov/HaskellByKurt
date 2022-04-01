import Data.Text (pack, toLower, unpack)

myGCD a b =
  if remainder == 0
    then b
    else myGCD b remainder
  where
    remainder = a `mod` b

-- myGCD2 a b = gcd' a b (a `mod` b)
-- where gcd' a b 0 = a
-- gcd' a b _ =  gcd' b (a `mod` b)

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  n -> "many"

myTail :: [t] -> [t]
myTail (_ : xs) = xs
myTail _ = []

simple :: [Int] -> [Int]
simple x = x

myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myLength2 [] = 0
myLength2 (x : xs) = 1 + myLength2 xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : myTake (n -1) xs

myCycle (first : rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (div n 2)
    else 1 + collatz (n * 3 + 1)

myReverse (x : xs) = myReverse xs ++ [x]
myReverse [] = []

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib n1 n2 0 = 0
fastFib n1 n2 counter = n1 + fastFib n2 (n1 + n2) (counter - 1)

myMap f [] = []
myMap f (x : xs) = (f x) : myMap f xs

myFilter f [] = []
myFilter f (x : xs) =
  if f x
    then x : myFilter f xs
    else myFilter f xs

myRemove f [] = []
myRemove f (x : xs) =
  if f x
    then myRemove f xs
    else x : myRemove f xs

myProduct res [] = res
myProduct res (x : xs) = myProduct (res * x) xs

myProduct2 list = foldl (*) 1 list

sumOfSqueres xs = foldl (+) 0 (map (^ 2) xs)

myFold f init [] = init
myFold f init (x : xs) = myFold f (f init x) xs

myFoldr f init [] = init
myFoldr f init (x : xs) = f x (myFoldr f init xs)

--9.1 Воспользуйтесь функциями filter и length, чтобы переопределить функцию elem.

myElem el [] = False
myElem el (x : xs) =
  if x == el
    then True
    else myElem el xs

myElem2 el xs = len /= 0
  where
    len = length (filter (== el) xs)

-- 9.2

isPalindrome [] = True
isPalindrome xs = processed == reverse processed
  where
    processed = unpack $ toLower $ pack $ filter (== ' ') xs

-- 1/2 + 1/3 + 1/4 + ...
harmonic n = harmonic' 0 n
  where
    harmonic' cum n =
      if n == 0
        then cum
        else harmonic' (cum + 1 / (n + 1)) (n -1)

main = do
  print $ myGCD 50 20
  print $ sayAmount 1
  print $ myTail [1, 2, 3]

  print $ myTail $ simple []

  -- print $ myGCD2 30 23

  print $ myLength [1, 2, 3]
  print $ myLength2 [1, 2, 3]

  print $ myTake 3 [1, 2, 3, 4, 5]

  print $ take 20 $ myCycle [1, 2, 3]

  print $ myReverse [1, 2, 3, 4, 5]

  print (fastFib 1 1 1000)

  print $ myMap (^ 2) [1, 2, 3, 4]

  print $ myFilter (== 1) [1, 2, 3, 1, 4]
  print $ myRemove (== 1) [1, 2, 3, 1, 4]

  print $ myProduct 1 [1, 2, 3, 4]
  print $ myProduct2 [1, 2, 3, 4]

  print $ sumOfSqueres [1, 2, 3]

  print $ myFold (*) 1 [1, 2, 3]

  print $ myElem 1 [1, 2, 3]
  print $ myElem2 'a' "abc"

  print "isPalindrome \"А роза упала на лапу Азора\""
  print $ isPalindrome "А роза упала на лапу Азора"

  print $ harmonic 2

  print $ myMap show [1, 2, 3, 4]