calcChange owed given = if change > 0
                        then change
                        else 0
    where change = given - owed

inc x = x + 1
double x = x * 2
square x = sqrt x

--main = do
--    calcChange 3 5

f2_3 x = if even x
         then x - 2
         else 3 * x + 1
