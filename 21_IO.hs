import Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hi, " ++ name ++ "!"

-- Задача 21.1. Перепишите код листинга 21.1 в код, использующий do-нотацию
-- в контексте Maybe. Предполагайте, что весь пользовательский ввод
-- заменён на Map, содержащий нужные значения. Пропустите первый вызов
-- putStrLn и просто верните приветствие в конце.

nameMap :: Map Int String
nameMap = fromList [(1, "Artem"), (2, "Mariya")]

getGreetings :: Maybe String
getGreetings = do
  name <- Map.lookup 1 nameMap
  let statement = helloPerson name
  return statement

main :: IO ()
main = do
  let greeting Nothing = ""
      greeting (Just a) = a
   in putStrLn $ greeting getGreetings