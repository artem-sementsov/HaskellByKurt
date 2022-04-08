--29.1. Используя <$> и <*>, скомбинируйте два значения
--типа Maybe String с помощью операции ++
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

hello :: IO String
hello = pure "Hello, world"

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

multiplictor :: [Int]
multiplictor = [10, 50]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

--Проверка 29.4. Решить эту задачу, считая, что коробочки представляют
--собой мультипликатор призовых: при открытии первой коробочки
--призовые увеличиваются в 10 раз, а при открытии второй — в 50.
totalPrize2 :: [Int]
totalPrize2 = pure (*) <*> doorPrize <*> multiplictor

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

data User = User
  { name :: String,
    gamerId :: Int,
    score :: Int
  }
  deriving (Show)

testNames :: [String]
testNames = ["Jon Smith", "Robert'); DROP TABLE Students;--", "Kristy NULL", "Rendal Monro"]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testData :: [User]
testData =
  pure User <*> testNames
    <*> testIds
    <*> testScores

--Задача 29.1. Чтобы доказать, что Applicative предоставляет больше возможностей,
-- чем Functor, напишите универсальную версию fmap под названием
-- allFmap, представляющую собой fmap для всех членов класса
-- типов Applicative. Так как она должна работать со всеми экземплярами
-- Applicative, можно пользоваться только его методами. Вот типовая
-- аннотация вашей функции:
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func arg = pure func <*> arg

-- Задача 29.2. Измените следующее выражение так, чтобы его результатом
-- было значение типа Maybe Int. Сложность состоит в том, что вы не можете
-- добавлять в код ничего, кроме pure и <*>. Вы не можете использовать
-- конструктор Just или дополнительные скобки.
-- example :: Int
-- example = (*) ((+) 2 4) 6
example :: Maybe Int
example = pure (*) <*> pure ((+) 2 4) <*> pure 6

-- Задача 29.3. Пользуясь приведёнными ниже данными и выполняя недетерминированные
-- вычисления со списками, определите, сколько пива вам
-- нужно купить, чтобы его наверняка хватило:
-- ‚ вчера вы купили пиво, но не помните, была ли это упаковка из 6 или
-- 12 бутылок;
-- ‚ прошлой ночью вы с соседом выпили по две бутылки пива;
-- ‚ сегодня к вам могут прийти двое или трое друзей, смотря как у них
-- будет получаться;
-- ‚ ожидается, что за ночь один человек выпьет три или четыре бутылки.
beerStock = [6, 12]

drinkWithNeighbor = [2 * 2]

friends = [2, 3]

beerConsumption = [3, 4]

beerNeeded = (-) <$> ((-) <$> [6, 12] <*> drinkWithNeighbor) <*> ((*) <$> map (+ 1) friends <*> beerConsumption)

main = do
  print $ (++) <$> Just "Hi, " <*> Just "world!"

  print $ pure (+) <*> [1, 2] <*> [3, 4]

  print totalPrize

  print totalPrize2

  print $ primesToN 32

  print $ length testData

  print $ allFmap (+ 1) [1, 2, 3]
  print $ allFmap (+ 1) (Just 5)
  print $ allFmap (+ 1) Nothing

  print $ example

  print "Beer needed:"
  print $ minimum beerNeeded