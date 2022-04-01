-- 6.1
myRepeat val = cycle [val]

--6.2
subseq start stop list = reverse $ drop stop $ reverse $ drop start list

--6.3
inFirstHalf el list = el `elem` take (length list `div` 2) list

main = do
    print $ take 10 $ myRepeat 5
    print $ subseq 2 5 [1..10]
    
    print $ inFirstHalf 'H' "Hello!"
    print $ inFirstHalf '!' "Hello!"
