
cup ml = (\message -> message ml)

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank = if mlDiff >= 0
                     then cup mlDiff
                     else cup 0
  where ml = getMl aCup
        mlDiff = ml - mlDrank

isEmpty aCup = getMl aCup == 0

afterManySips = foldl drink coffeeCup [30, 30, 30, 30, 30]

coffeeCup = cup 500

-- robots
robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

killerRobot = robot ("killer", 25, 200)

nicerRobot = setName killerRobot "Kitten"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

printRobot aRobot = aRobot (\(n, a, h) ->
                              n ++
                              " attack:" ++ (show a) ++
                              " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0

gentleGiant = robot ("Gentle", 10, 300)

robots = [nicerRobot, gentlerRobot, softerRobot]



-- threeRoundFight = nRoundFight 3

-- compareHp aRobot bRobot
--   | (getHP aRobot) > (getHP bRobot) = aRobot
--   | otherwise = bRobot

-- nRoundFight 0 robotA robotB
--   | (getHP robotA) > (getHP robotB) = robotA
--   | otherwise = robotB
-- nRoundFight n robotA robotB = nRoundFight (n-1) (fight robotB robotA) (fight robotA robotB)
-- nRoundFight n robotA robotB = nRoundFight (n-1) robotA robotB


main = do
    print $ getAttack killerRobot

    --10.1 используя map на списке роботов, получите количество жизни каждого робота в списке
    print $ map getHP robots

    -- print $ printRobot $ compareHp gentleGiant killerRobot

    -- print $ printRobot $ threeRoundFight nicerRobot gentlerRobot
    -- print $ printRobot $ nRoundFight 0 nicerRobot gentlerRobot
    -- print $ printRobot nicerRobot
    