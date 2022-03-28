module Main where

import Lib
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Введите слово, чтобы узнать, является ли оно палиндромом."
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "Да, это палиндром."
                   else "Нет, это не палиндром."
    TIO.putStrLn response