import qualified Data.Map as Map

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just string) = Just (reverse string)

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "левая рука для того, чтобы бить в лицо!",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "правая рука для добрых жестов",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "эта голова выглядит безумной",
      cost = 5092.25,
      count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Задача 27.1. Когда мы представили параметризованные типы в уроке 15,
-- то использовали минимальный тип Box в качестве примера:
-- data Box a = Box a deriving Show
-- Реализуйте класс типов Functor для типа Box. Затем реализуйте функцию
-- morePresents, которая меняет тип с Box a на Box [a], который содержит
-- n копий исходного значения в списке. Убедитесь, что использовали fmap
-- для реализации.
data Box a = Box a deriving (Show)

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (replicate n)

-- Задача 27.2. Теперь предположим, что у вас есть простая коробка вроде этой:
-- myBox :: Box Int
-- myBox = Box 1
-- Используйте fmap, чтобы переложить ваше значение в коробке в другую коробку.
-- Затем определите функцию unwrap, которая вынимает значение из
-- коробки, и используйте fmap на этой функции, чтобы получить вашу первоначальную
-- коробку. Вот как ваш код должен работать в GHCi:
-- GHCi> wrapped = fmap ? myBox
-- GHCi> wrapped
-- Box (Box 1)
-- GHCi> fmap unwrap wrapped
-- Box 1

myBox :: Box Int
myBox = Box 1

unwrap :: Box Int -> Int
unwrap (Box val) = val

wrap :: a -> Box a
wrap = Box

-- Задача 27.3. Напишите интерфейс командной строки для базы данных
-- компонентов роботов partsDB, который позволит пользователю искать
-- стоимость предмета по его идентификатору. Используйте тип Maybe для
-- обработки случая, когда пользователь запрашивает отсутствующий предмет.

lookupPartsDB :: Int -> Maybe Double
lookupPartsDB index = fmap cost (Map.lookup index partsDB)

fullName :: String -> String -> String
fullName first last = first ++ " " ++ last

main = do
  print $ incMaybe successfulRequest
  print $ incMaybe failedRequest

  print $ fmap (+ 1) successfulRequest
  print $ fmap (+ 1) failedRequest

  print $ show <$> successfulRequest

  print $ reverse <$> Just "abc"

  print $ fmap (\x -> if x == 5 then "five" else "unknown") (Box 5)

  print $ morePresents 10 (Box "3")

  print $ fmap unwrap (fmap wrap myBox)

  print $ lookupPartsDB 1
  print $ lookupPartsDB 100

  print $ fullName "first" "last"
  print $ fmap (fullName "first") (Just "last")
  print $ fmap (fullName "first") Nothing
