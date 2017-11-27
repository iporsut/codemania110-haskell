quickSort [] = []
quickSort (p:xs) = quickSort [x | x <- xs, x <= p]
                   ++ [p]
                   ++ quickSort [x | x <- xs, x > p]
