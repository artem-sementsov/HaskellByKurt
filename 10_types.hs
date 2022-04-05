{-# LANGUAGE DeriveAnyClass #-}

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

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(Age: " ++ show age ++ "; height: " ++ show height ++ "sm)"

patientInfo2 :: PatientName -> Int -> Int -> String
patientInfo2 (fname, lname) age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(Age: " ++ show age ++ "; height: " ++ show height ++ "sm)"

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male = "male"
showSex Female = "female"

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "Джон" "Доу") Male 43 188 92 (BloodType AB Pos)

jineSmith :: Patient
jineSmith = Patient (NameWithMiddle "Jine" "Elizabeth" "Smith") Female 38 173 68 (BloodType O Neg)

data Patient2 = Patient2
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

jackieSmith :: Patient2
jackieSmith =
  Patient2
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 157,
      weight = 52,
      bloodType = BloodType O Neg
    }

jackieSmithUpdated = jackieSmith {age = 44}

--12.1
canDonateTo2 :: Patient2 -> Patient2 -> Bool
canDonateTo2 patient1 patient2 = canDonateTo (bloodType patient1) (bloodType patient2)

--12.2
patientSummary :: Patient2 -> String
patientSummary patient =
  "**************"
    ++ "Patient name: "
    ++ showName (name patient)
    ++ "\nSex: "
    ++ showSex (sex patient)
    ++ "\nAge: "
    ++ show (age patient)
    ++ "\nHeight: "
    ++ show (height patient)
    ++ "sm"
    ++ "\nWeight: "
    ++ show (weight patient)
    ++ "kg"
    ++ "\nBlood type: "
    ++ showBloodType (bloodType patient)
    ++ "\n**************"

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

--13.3
cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc = toEnum . (+ 1) . fromEnum

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord, Enum)

-- instance Show SixSidedDie where
--   show S1 = "one (I)"
--   show S2 = "two (II)"
--   show S3 = "three (III)"
--   show S4 = "four (IV)"
--   show S5 = "five (V)"
--   show S6 = "six (VI)"

-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "No such value"
--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5

newtype Name2 = Name2 (String, String) deriving (Show, Eq)

instance Ord Name2 where
  compare (Name2 (f1, l1)) (Name2 (f2, l2)) = compare (l1, f1) (l2, f2)

--14.1
data T14_1 = X | Y | Z deriving (Show, Enum)

instance Eq T14_1 where
  (==) v1 v2 = (==) (fromEnum v1) (fromEnum v2)

instance Ord T14_1 where
  compare v1 v2 = compare (fromEnum v1) (fromEnum v2)

--14.2
data FiveSidedDie = F1 | F2 | F3 | F4 | F5 deriving (Show, Enum, Eq, Ord, Die)

class (Show a, Ord a, Eq a, Enum a) => Die a where
  getRomeNumber :: a -> String
  getRomeNumber die = case fromEnum die of
    0 -> "I"
    1 -> "II"
    2 -> "III"
    _ -> "many"

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset = if even n then fromEnum c + halfN else 1 + fromEnum c + halfN
    rotation = offset `mod` n

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder = map rot4l
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3l
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot3ldecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder = map rotChar
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotCharDecoder = rotNdecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && not (value1 && value2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - length reversedBits
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size = length bits
    indices = [size -1, size -2 .. 0]
    trueLocations = filter fst (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP :: String -> String -> String
applyOTP pad = applyOTPBits padBits
  where
    padBits = map charToBits pad

applyOTPBits :: [Bits] -> String -> String
applyOTPBits bits plaintext = map bitsToChar bitList
  where
    bitList = map (\pair -> (fst pair) `xor` (snd pair)) (zip bits plaintextBits)
    plaintextBits = map charToBits plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

defaultPrngGen :: Int -> Int
defaultPrngGen = prng 13327 13 1000

prngList :: Int -> [Int]
prngList seed = list
  where
    list = defaultPrngGen seed : map defaultPrngGen list

prngBits :: Int -> [Bits]
prngBits seed = map intToBits (prngList seed)

data StreamCipher = SC [Bits]

instance Cipher StreamCipher where
  encode (SC bits) text = applyOTPBits bits text
  decode (SC bits) text = applyOTPBits bits text

mySC :: StreamCipher
mySC = SC $ prngBits 13

main = do
  print $ halve 5

  print $ printDouble 3

  print $ patientInfo "Jon" "Doy" 43 188

  print $ patientInfo2 ("first", "last") 43 188

  print $ height jackieSmith

  print $ showName $ name jackieSmith

  print $ patientSummary jackieSmith

  print $ Vanilla > Chocolate
  print $ Vanilla < Chocolate

  print $ cycleSucc 'a'
  print $ cycleSucc LT
  print $ cycleSucc False

  print $ S6 == S6
  print $ S6 /= S6
  print $ S6 > S5

  print [F1, F2]

  print $ getRomeNumber F3

  print $ rotN 4 L1

  print $ fourLetterEncoder message

  print $ rotDecoder (rotEncoder "hi")
  print $ rotDecoder (rotEncoder "Jean-Paul likes Simone")
  print $ rotDecoder (rotEncoder "123")

  print $ encode Rot "Haskell"
  print $ decode Rot "\557128\557153\557171\557163\557157\557164\557164"

  print $ encode myOTP "Learn Haskell"
  print $ decode myOTP "Ldcqj%Nf{bog`"

  print $ take 10 $ prngList 5

  print $ encode mySC "Learn Haskell"
  print $ decode mySC "\324\304\617G\310\469\768\724\515V\933\969\EOT"
