import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkhem", (42.6054, -70.7829)),
      ("Innsmut", (42.8250, -70.8150)),
      ("Karkoza", (29.9714, -90.7694)),
      ("New-York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 6378.1

-- Задача 28.1. Реализация haversineMaybe была достаточно простой. Теперь
-- попробуйте написать функцию haversineIO без использования <*>.
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO start dest = haversine <$> start <*> dest

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, city does not exist in database"
printDistance (Just dist) = putStrLn (show dist ++ " km")

-- Напишите функцию addMaybe, вычисляющую сумму двух значений типа Maybe Int.
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a + b)
addMaybe _ _ = Nothing

maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

-- Проверка 28.3. Используя шаблон из примера для двухаргументных
-- функций, передайте функциям (*), div и mod эти два значения:
val1 = Just 10

val2 = Just 5

startingCity = Map.lookup "Karkoza" locationDB

destCity = Map.lookup "Innsmut" locationDB

main = do
  print $ haversine (40.7776, -73.9691) (42.6054, -70.7829)

  print $ maybeInc <*> Just 5
  print $ maybeInc <*> Nothing

  print $ (++) <$> Just "cats" <*> Just " and dogs"
  print $ (++) <$> Nothing <*> Just " and dogs"

  print $ (*) <$> val1 <*> val2
  print $ div <$> val1 <*> val2
  print $ mod <$> val1 <*> val2

  let distance = haversine <$> startingCity <*> destCity
  printDistance distance