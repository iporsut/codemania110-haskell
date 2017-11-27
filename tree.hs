data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

add :: Ord a => a -> Tree a -> Tree a
add a Empty = Node a Empty Empty
add a (Node n l r) | a <= n    = Node n (add a l) r
                   | otherwise = Node n l (add a r)

dfsList :: Ord a => Tree a -> [a]
dfsList Empty = []
dfsList (Node a l r) = dfsList l ++ [a] ++ dfsList r

addList :: Ord a => [a] -> Tree a
addList xs = foldr add Empty xs

sort :: Ord a => [a] -> [a]
sort xs = dfsList (addList xs)
-- sort xs = dfsList $ addList xs
-- sort xs = dfsList . addList $ xs

searchTree :: Ord a => a -> Tree a -> Maybe a
searchTree a Empty = Nothing
searchTree a (Node n l r) | a == n    = Just a
                          | a <= n    = searchTree a l
                          | otherwise = searchTree a r

search :: Ord a => a -> [a] -> Maybe a
search a xs = searchTree a (addList xs)
-- search a xs = searchTree a $ addList xs
-- search a = searchTree a  . addList
