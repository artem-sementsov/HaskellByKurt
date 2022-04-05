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

type Events = [String]

type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
    combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["orel", "reshka"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

main = do
  print $ map price items
  print $ map madeBy items

  print $ map area shapes
  print $ map perimert shapes

  print $ Red <> Yellow

  print $ createPTable ["orel", "reshka"] [0.5, 0.5]

  print $ coin <> spinner
  print $ mconcat [coin, coin, coin]

--17.1
  print $ Transparent <> Red
  print $ Blue <> Transparent
  print $ mconcat [Blue, Red]
  print $ mconcat [Blue, Transparent, Red]
  print $ mconcat [Blue, Transparent, Red, Purple]
  