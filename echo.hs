echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose =
  putStrLn "Enter line, we will repeat it!" >> getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What's your name?"

nameStatement :: String -> String
nameStatement name = "Hi, " ++ name ++ "!"

main :: IO ()
main = (askForName >> getLine) >>= return . nameStatement >>= putStrLn