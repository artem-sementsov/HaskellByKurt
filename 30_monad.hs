import qualified Data.Map as Map

type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep"),
      (2, "KINGinYELLOW"),
      (3, "dagon1997"),
      (4, "rcarter1919"),
      (5, "xCTHULHUx"),
      (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000),
      ("KINGinYELLOW", 15000),
      ("dagon1997", 300),
      ("rcarter1919", 12),
      ("xCTHULHUx", 50000),
      ("yogSOThoth", 150000)
    ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromIdAlt :: GamerId -> Maybe PlayerCredits
creditsFromIdAlt id = altLookupCredits (lookupUserName id)

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001, 1), (1002, 2), (1003, 3), (1004, 4), (1005, 5), (1006, 6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

testFun :: Num a => a -> a
testFun = (+ 2)

funToIO :: Num a => a -> IO a
funToIO = return . testFun

-- Задача 30.1. Чтобы доказать, что Monad строго мощнее Functor, напишите
-- универсальную версию <$> под названием allFmapM, которая определит
-- <$> для всех членов класса типов Monad. Так как она должна работать
-- для всех экземпляров Monad, вы можете использовать только методы
-- из определения класса Monad (и лямбда-функции). Вот тип этой функции
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func a = a >>= (return . func)

-- Задача 30.2. Чтобы доказать, что Monad строго мощнее Applicative, напишите
-- универсальную версию <*> под названием allApp, которая определит
-- <*> для всех членов класса типов Monad. Так как она должна работать
-- для всех экземпляров Monad, вы можете использовать только методы
-- из определения класса Monad (и лямбда-функции). Вот тип этой функции:
-- allApp :: Monad m => m (a -> b) -> m a -> m b
-- allApp func a =

-- Задача 30.3. Реализуйте аналогичную >>= функцию для Maybe:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing func = Nothing
bind (Just val) func = func val

main = do
  print $ creditsFromId 1
  print $ creditsFromId 100

  print $ creditsFromWCId 1001
  print $ creditsFromWCId 10

-- readInt >>= printDouble