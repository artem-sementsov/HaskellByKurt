module Main where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

myToken :: BC.ByteString
myToken = "sZjNYYzWHjixXDLWZeQGQIXURIojcDxf"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestPath path
  $ setRequestPort 443
  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

requestNoSSL :: Request
requestNoSSL = buildRequestNOSSL "8.8.8.8" "GET" ""

main :: IO ()
main = do
    -- response <- httpLBS requestNoSSL
    -- print response
    response <- httpLBS request
    -- print response
    let status = getResponseStatus response
    if (statusCode status) == 200
        then do
            putStrLn "Results have been saved into file"
            let jsonBody = getResponseBody response
            L.writeFile "data.json" jsonBody
        else
            putStrLn $ "Request finished with error:" ++ (BC.unpack $ statusMessage status)
