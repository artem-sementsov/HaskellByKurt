module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Tool = Tool { toolId :: Int
                 , name :: String
                 , description :: String
                 , lastReturned :: Day
                 , timesBorrowed :: Int
                 }

data User = User { userId :: Int
                 , userName :: String
                 }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user]

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n описание: "
                        , description tool
                        , "\n последний раз возвращено: "
                        , show $ lastReturned tool
                        , "\n количество раз сдано в аренду: "
                        , show $ timesBorrowed tool
                        , "\n"]

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

addUser :: String -> IO ()
addUser userName = do
    withConn "tools.db" $
      \conn -> do
        execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
        putStrLn "пользователь добавлен"

addTool :: String -> String -> Day -> Int -> IO ()
addTool name description lastReturned timesBorrowed = do
    withConn "tools.db" $
      \conn -> do
        execute conn "INSERT INTO tools (name, description, lastReturned, timesBorrowed) VALUES (?, ?, ?, ?)" (name, description, lastReturned, timesBorrowed)
        putStrLn "инструмент добавлен"

checkout :: Int -> Int -> IO ()
checkout userId toolId =
    withConn "tools.db" $
      \conn ->
        execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)" (userId, toolId)

instance FromRow User where
    fromRow = User <$> field
                   <*> field
instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

printUsers :: IO ()
printUsers =
    withConn "tools.db" $
      \conn -> do
        resp <- query_ conn "SELECT * FROM users;" :: IO [User]
        mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
                   \conn -> do
                       resp <- query_ conn q :: IO [Tool]
                       mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
    mconcat [ "select * from tools "
            , "where id not in "
            , "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
    mconcat [ "select * from tools "
            , "where id in "
            , "(select tool_id from checkedout);"]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
                  "SELECT * FROM tools WHERE id = (?)"
                  (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp
    
firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "id не был найден"
updateOrWarn (Just tool) =
    withConn "tools.db" $
      \conn -> do
        let q = mconcat [ "UPDATE TOOLS SET "
                        , "lastReturned = ?,"
                        , " timesBorrowed = ? "
                        , "WHERE ID = ?;"]
        execute conn q (lastReturned tool,
                        timesBorrowed tool,
                        toolId tool)
        putStrLn "данные об инструменте обновлены"

updateToolTable :: Int -> IO ()
updateToolTable toolId =
    withConn "tools.db" $
      \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime
        let updatedTool = updateTool <$> tool
                                     <*> pure currentDay
        updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId =
  withConn "tools.db" $ \conn ->
      execute conn
              "DELETE FROM checkedout WHERE tool_id = (?);"
              (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    putStrLn "Введите имя нового пользователя:"
    userName <- getLine
    addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
    putStrLn "Введите название инструмента:"
    name <- getLine
    putStrLn "Введите описание инструмента:"
    description <- getLine
    currentDay <- utctDay <$> getCurrentTime
    addTool name description currentDay 0

promptAndCheckout :: IO ()
promptAndCheckout = do
    putStrLn "Введите ID пользователя:"
    userId <- pure read <*> getLine
    putStrLn "Введите ID инструмента:"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    putStrLn "Введите ID инструмента:"
    toolId <- pure read <*> getLine
    checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand "users" = printUsers >> main
performCommand "tools" = printTools >> main
performCommand "adduser" = promptAndAddUser >> main
performCommand "addtool" = promptAndAddTool >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin" = promptAndCheckin >> main
performCommand "in" = printAvailable >> main
performCommand "out" = printCheckedout >> main
performCommand "quit" = putStrLn "Чао!"
performCommand _ = putStrLn "Команда не найдена" >> main

main :: IO ()
main = do
    putStrLn "Введите команду:"
    command <- getLine
    performCommand command
