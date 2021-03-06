module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

data Book = Book
    { title :: T.Text
    , author :: T.Text
    , year :: Int
    } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author="Will Curt"
              , title="Programming on Haskell"
              , year=2019}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Choran\",\"title\":\"Tractat o razlozhenii\",\"year=1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

data ErrorMessage = ErrorMessage
    { message :: T.Text
    , errorCode :: Int
    } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) =
        ErrorMessage <$> v .: "message"
                     <*> v .: "error"

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) =
        object [ "message" .= message
               , "error" .= errorCode
               ]

data Name = Name
    { firstName :: T.Text
    , lastName :: T.Text
    } deriving (Show)

instance FromJSON Name where
    parseJSON (Object v) =
        Name <$> v .: "firstName"
             <*> v .: "lastName"

instance ToJSON Name where
    toJSON (Name firstName lastName) =
        object [ "firstName" .= firstName
               , "lastName" .= lastName
               ]

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Choran\",\"title\": \"Tractat o razlozhenii\",\"year\"=1949}"
bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

sampleError :: BC.ByteString
sampleError = "{\"message\":\"wow!\",\"error\": 123}"

exampleMessage :: Maybe T.Text
exampleMessage = Just "wow!"
exampleError :: Maybe Int
exampleError = Just 123

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Good" 0

data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double
                             , resultId :: T.Text
                             } deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult <$> v .: "uid"
                   <*> v .: "mindate"
                   <*> v .: "maxdate"
                   <*> v .: "name"
                   <*> v .: "datacoverage"
                   <*> v .: "id"

data Resultset = Resultset { offset :: Int
                           , count :: Int
                           , limit :: Int
                           } deriving (Show,Generic)
instance FromJSON Resultset

data Metadata = Metadata { resultset :: Resultset
                         } deriving (Show,Generic)
instance FromJSON Metadata

data NOAAResponse = NOAAResponse
                  { metadata :: Metadata
                  , results :: [NOAAResult]
                  } deriving (Show,Generic)
instance FromJSON NOAAResponse

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left err) = BC.putStrLn (BC.pack err)
printResults (Right results) = forM_ results (print . name)

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults
