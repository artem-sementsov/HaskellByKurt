-- Задача 21.2. Напишите программу, запрашивающую у пользователя чис-
-- лоn и возвращающую n-е число Фибоначчи (пример вычисления чисел Фибоначчи
-- можно найти в уроке 8).

fibonachi :: Int -> Int
fibonachi 0 = 1
fibonachi 1 = 1
fibonachi n = fibonachi (n -1) + fibonachi (n -2)

main = do
  putStrLn "Enter n for fibonachi"
  n <- getLine
  let res = fibonachi $ read n
  putStrLn $ "Fibonachi for " ++ n ++ " is " ++ show res