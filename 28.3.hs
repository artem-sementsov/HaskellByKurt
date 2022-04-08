--28.3
import Data.Map as Map

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

robotBody :: RobotPart
robotBody =
  RobotPart
    { name = "robot body",
      description = "just meat",
      cost = 25000.0,
      count = 1
    }

robotFinger :: RobotPart
robotFinger =
  RobotPart
    { name = "robot finger",
      description = "only one finger",
      cost = 10,
      count = 20
    }

rpDB :: Map.Map Int RobotPart
rpDB = Map.fromList $ zip [1, 2, 3, 4, 5] [robotFinger, leftArm, rightArm, robotHead, robotBody]

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

readId :: IO Int
readId = read <$> getLine

minByCost :: RobotPart -> RobotPart -> RobotPart
minByCost a b =
  if cost a < cost b
    then a
    else b

dbLookup :: Int -> Maybe RobotPart
dbLookup id = Map.lookup id rpDB

main :: IO ()
main = do
  putStrLn "Enter 2 ids of robot parts"
  id1 <- readId
  id2 <- readId
  let minCostRobot = minByCost <$> dbLookup id1 <*> dbLookup id2

  print minCostRobot
