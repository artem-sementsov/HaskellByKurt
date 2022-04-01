import Data.List

ifEven myFunction x = if even x
                      then myFunction x
                      else x

inc x = x + 1
double x = x * 2
square x = x ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n


ifEvenInc2 = ifEven inc
ifEvenDouble2 = ifEven double
ifEvenSquare2 = ifEven square


ifEvenCube n = ifEven (\x -> x ^ 3) n

author = ("Will", "Kurt")

names = [("Иэн","Кертис"),
         ("Бернард","Самнер"),
         ("Питер","Хук"),
         ("Билл","Хук"),
         ("Стивен","Моррис")]

compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2

compareLastNames2 name1 name2 = if lastNameRes /= EQ
                                then lastNameRes
                                else firstNameRes
  where lastNameRes = compare lastName1 lastName2
        firstNameRes = compare firstName1 firstName2
        lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2

sfOffice name =
    if lastName < "Л"
    then nameText ++ " - А/я 1234, Сан-Франциско, штат Калифорния, 94111"
    else nameText ++ " - А/я 1010, Сан-Франциско, штат Калифорния, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": А/я 789, Нью-Йорк, штат Нью-Йорк, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - А/я 456, Рино, штат Невада, 89523"
  where nameText = snd name

colOffice name = "Многоуважаемый(ая) " ++ nameText
  where nameText = (fst name) ++ " " ++ (snd name)

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

addressLetterV2 location name = addressLetter name location
addressLetterV3 = flipBinaryArgs addressLetter

addressLetterNY = addressLetterV2 "ny"

flipBinaryArgs f = (\a b -> f b a)

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "col" -> colOffice
    _ -> (\name -> fst name ++ " " ++ snd name)

main = return ()