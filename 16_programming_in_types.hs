import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (isNothing)

data AuthorName = AuthorName
  { firstName :: String,
    lastname :: String
  }

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy (ToyItem toy) = "unknown"
madeBy (PamphletItem pamphlet) = organizationContact pamphlet

someCreator = AuthorCreator (Author (Name "X" "Y"))

bookItem = Book someCreator "" "" 1 2.0

recordItem = VinylRecord someCreator "" 0 0.0

pamphletItem = Pamphlet "" "some organization"

items :: [StoreItem]
items = [BookItem bookItem, RecordItem recordItem, PamphletItem pamphletItem]

--16.1
data Pamphlet = Pamphlet
  { pamphletDescription :: String,
    organizationContact :: String
  }

--16.2
data Circle = Circle
  { radius :: Double
  }

data Square = Square
  { squareLength :: Double
  }

data Rectangle = Rectangle
  { rectangleLength :: Double,
    rectangleHeigth :: Double
  }

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimert :: Shape -> Double
perimert (CircleShape circle) = 2 * 3.14 * radius circle
perimert (SquareShape square) = 4 * squareLength square
perimert (RectangleShape rectangle) = 2 * rectangleLength rectangle + 2 * rectangleHeigth rectangle

area :: Shape -> Double
area (CircleShape circle) = 3.14 * radius circle ^ 2
area (SquareShape square) = squareLength square ^ 2
area (RectangleShape rectangle) = rectangleLength rectangle * rectangleHeigth rectangle

shapes :: [Shape]
shapes = [CircleShape (Circle 1.0), SquareShape (Square 1.0), RectangleShape (Rectangle 1.0 2.0)]

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldl (||) False . map pred

instance Semigroup Integer

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Transparent deriving (Show, Eq)

instance Semigroup Color where
  (<>) Transparent b = b
  (<>) a Transparent = a
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = Transparent
  mappend = (<>)

howMuch :: Int -> String
howMuch n
  | n > 10 = "целая куча"
  | n > 0 = "немного"
  | otherwise = "мы в долгах!"

data Events = Events
  { eventsVal :: [String]
  }
  deriving (Show)

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mempty = Events []
  mappend = combineEvents

data Probs = Probs
  { probsVal :: [Double]
  }
  deriving (Show)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mempty = Probs []
  mappend = combineProbs

data PTable = PTable Events Probs

p = Probs []

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events (Probs normalizedProbs)
  where
    totalProbs = sum $ probsVal probs
    normalizedProbs = map (/ totalProbs) $ probsVal probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair (eventsVal events) $ probsVal probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = Events $ cartCombine combiner (eventsVal e1) (eventsVal e2)
  where
    combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = Probs $ cartCombine (*) (probsVal p1) (probsVal p2)

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

coin :: PTable
coin = createPTable (Events ["orel", "reshka"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])

data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

aBox :: Box Double
aBox = Box 2.0

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Говард" "Филлипс" "Лавкрафт"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

data List a = Empty | Cons a (List a) deriving (Show)

builtinEx1 :: [Int]
builtinEx1 = 1 : 2 : 3 : []

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'к' : 'о' : 'т' : []

ourListEx2 :: List Char
ourListEx2 = Cons 'к' (Cons 'о' (Cons 'т' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

itemCount1, itemCount2, itemCount3 :: (String, Int)
itemCount1 = ("Стёрки", 25)
itemCount2 = ("Карандаши", 25)
itemCount3 = ("Ручки", 13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

instance Ord Organ where
  (<=) a b = show a <= show b

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--18.1 Реализуйте аналогичные map функции tripleMap и boxMap для типов Box и Triple.

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple a1 a2 a3) = Triple (f a1) (f a2) (f a3)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

--18.2 Модифицируйте тип Organ так, чтобы он мог быть использован в качестве ключа. Затем постройте Map под названием organInventory,
--где сам орган будет ключом, а количество таких органов в organCatalog — значением.

organInventory :: Map.Map Organ Int
organInventory = Map.fromListWith (+) (zip organs (repeat 1))

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = (`Map.lookup` catalog)

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (Just organ ==) available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- Проверка 19.2. Напишите функцию numOrZero, которая принимает Maybe Int и возвращает 0, если это Nothing, и число в противном случае.
numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just a) = a

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in vat"
  show (Cooler organ) = show organ ++ " in cooler"
  show (Bag organ) = show organ ++ " in bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) =
  show container
    ++ " (place: "
    ++ show location
    ++ ")"

report' :: Maybe (Location, Container) -> String
report' Nothing = "error, nothing received"
report' (Just (location, container)) =
  show container
    ++ " (place: "
    ++ show location
    ++ ")"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where
    organ = Map.lookup id catalog

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id was not found"

--Задача 19.1. Реализуйте функцию emptyDrawers, принимающую результат вызова функции geDrawerContents
-- и возвращающую количество пустых шкафчиков.
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers contents = length (filter isNothing contents)

--Задача 19.2. Реализуйте аналогичную функции map функцию под названием maybeMap, работающую с типами Maybe.
maybeMap :: (Maybe a -> Maybe b) -> [Maybe a] -> [Maybe b]
maybeMap = map

main = do
  print $ map price items
  print $ map madeBy items

  print $ map area shapes
  print $ map perimert shapes

  print $ Red <> Yellow

  print coin

  print $ coin <> spinner
  print $ mconcat [coin, coin, coin]

  --17.1
  print $ Transparent <> Red
  print $ Blue <> Transparent
  print $ mconcat [Blue, Red]
  print $ mconcat [Blue, Transparent, Red]
  print $ mconcat [Blue, Transparent, Red, Purple]

  --17.2
  print $ Events ["1", "2"]
  print $ Probs [0.4, 0.6]
  print $ mconcat [Probs [0.4, 0.6], Probs [0.2, 0.8]]

  print $ transform reverse aPerson
  print $ transform toLower initials
  print $ toList (transform toLower initials)

  print $ ourMap (* 2) ourListEx1

  print $ Map.lookup 7 organCatalog

  --18.1
  print $ tripleMap show aPoint
  print $ boxMap show aBox

  --18.2
  print organInventory
  print $ Map.lookup Heart organInventory

  print $ countOrgan Brain availableOrgans
  print $ countOrgan Heart availableOrgans

  print justTheOrgans
  print $ map showOrgan justTheOrgans

  print $ showOrgan (Just Heart)
  print $ showOrgan Nothing

  print cleanList

  print $ process Brain
  print $ process Heart
  print $ process Spleen
  print $ process Kidney
  print $ report (process Brain)
  print $ report (process Spleen)

  print $ processRequest 13 organCatalog
  print $ processRequest 12 organCatalog

  print $ emptyDrawers (getDrawerContents possibleDrawers organCatalog)

  print $ maybeMap (\x -> if isNothing x then Just 0 else Nothing) [Nothing, Just "a", Nothing, Just "Hi"]