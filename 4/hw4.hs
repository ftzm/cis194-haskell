module HW4 where

nums :: [Integer]
nums = [1,2,3,4,5,6,7,8,9]

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node h t1 v t2)
  | h1 > h2 = Node h t1 v nt2
  | h1 < h2 = Node h nt1 v t2
  | hnt1 > hnt2 = Node h t1 v nt2
  | otherwise = Node (hnt1+1) nt1 v t2
  where
    h1 = treeHeight t1
    h2 = treeHeight t2
    nt1 = treeInsert x t1
    nt2 = treeInsert x t2
    hnt1 = treeHeight nt1
    hnt2 = treeHeight nt2

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node x _ _ _) = x

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

xor' :: [Bool] -> Bool
xor' = odd . length . filter (==True)

xor :: [Bool] -> Bool
xor = foldl (\acc x -> if x then not (x && acc) else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((1+) . (*2)) $ exclude [1..n] exclusions
  where
    exclude xs ys = [ x | x <- xs, x `notElem` ys]
    exclusions = [ x+y+(2*x*y) | x <- [1..n], y <- [2..n], x <= y, x+y+(2*x*y) <= n]
