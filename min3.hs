minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

data User = User
  { name :: String,
    gamerId :: Int,
    score :: Int
  }
  deriving (Show)

serverUsername :: Maybe String
serverUsername = Just "Сью"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001



main :: IO ()
main = do
  -- Проверка 28.4. Используя minOfThree, получите значение Maybe Int
  -- из этих трёх значений в контексте Maybe:
  -- Just 10
  -- Just 3
  -- Just 6
  print $ minOfThree (Just 10) (Just 3) (Just 6)

  --   putStrLn "Enter 3 numbers"
  --   minInt <- minOfInts
  --   putStrLn (show minInt ++ " is min")

  print $ User <$> serverUsername <*> serverGamerId <*> serverScore
  print $ User <$> Nothing <*> serverGamerId <*> serverScore

  putStrLn "Enter username, ID and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user