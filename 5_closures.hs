
ifEven myFunction x = if even x
                      then myFunction x
                      else x

inc x = x + 1

genIfEven f = (\x -> ifEven f x)

ifEvenInc = genIfEven inc

genIfEvenX x = (\f -> ifEven f x)

genIfEven_2 = genIfEvenX 2

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

genApiRequestBuilder2 hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

myImplApiRequestBuilder2 = genApiRequestBuilder2 exampleUrlBuilder "1337hAsk3ll" "book"

exampleUrlBuilder2 = getRequestUrl "http://example.com"
myExampleUrlBuilder2 = exampleUrlBuilder "1337hAsk3ll"

myImpl_5_3 = getRequestUrl "http://example.com" "1337hAsk3ll" "book"

subtract2 = flip (-) 2

binaryPartialApplication f x = (\y -> f x y)

main = return ()

