import Prelude hiding (filter)

halve = flip div 2

printDouble :: Int -> String
printDouble = show . (* 2)

makeAddress number street town = (number, street, town)

makeAddressNumber :: street -> town -> (Integer, street, town)
makeAddressNumber = makeAddress 12

makeAddressNumberStreet :: String -> (Integer, String, String)
makeAddressNumberStreet = makeAddress 12 "cebici"

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
    else n

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

--11.1
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
  | f x = x : tail
  | otherwise = tail
  where
    tail = filter f xs

-- 11.2
myTail :: [a] -> [a]
myTail [] = []
myTail xs = tail xs

-- head does not work on [], cause of head :: [a] -> a

-- 11.3
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

main = do
  print $ halve 5

  print $ printDouble 3