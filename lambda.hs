
doubleDouble x = dubs * 2
    where dubs = x * 2

doubleDouble2 x = (\dubs -> dubs * 2) x * 2

overwrite x = let x = 2
              in let x = 3
               in let x = 4
                in x

overwrite2 x = (\x -> (\x -> (\x -> x) 4) 3) 2

sumSquareOrSquareSum x y = (\sumSquare squareSum -> 
                           if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x^2 + y^2) ((x + y)^2)

counter x = let x = x + 1
            in
             let x = x * 2
             in
              x

counter2 x = (\x -> (\x -> x + 1) x * 2) x
counter3 x = (\x -> x + 1) ((\x -> x * 2) x)

main :: IO ()
main = return ()
