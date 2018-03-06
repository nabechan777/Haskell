
import Data.List

myList :: [a] -> a
myList [x] = x
myList (_:xs) = myList xs

myButLast :: [a] -> a
myButLast xs = reverse xs !! 1

elementAt :: [a] -> Int -> a
elementAt list i = list !! (i - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

compress :: Eq a => [a] -> [a]
compress = map head . group

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let
    (first, rest) = span (== x) xs
    in (x:first) : pack rest

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

data ListItem a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple l x) = replicate l x

encode' :: Eq a => [a] -> [(Int, a)]
encode' = foldr helper []
    where
        helper x [] = [(1, x)]
        helper x (y@(a,b):ys)
            | x == b = (1+a, x):ys
            | otherwise = (1, x):y:ys

encodeDirected :: Eq a => [a] -> [ListItem a]
encodeDirected = map encodeHelper . encode'
    where
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

dpli :: [a] -> [a]
dpli [] = []
dpli (x:xs) = x:x:dpli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

myDrop :: [a] -> Int -> [a]
myDrop xs n = helper xs n
    where
        helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) k = x : helper xs (k-1)

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split l@(x:xs) n | n > 0 = (x : ys, zs)
                 | otherwise = ([], l)
    where
        (ys, zs) = split xs (n - 1)

slice :: [a] -> Int -> Int -> [a]
slice xs i k | i > 0 = take (k-i+1) $ drop (i-1) xs

rotate :: [a] -> Int -> [a]
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
    where
        len = length xs

main :: IO ()
main = do
    print $ myList [1..9]
    print $ myButLast [1..9]
    print $ elementAt [1, 2, 3] 2
    print $ myLength "Hello World!"
    print $ myReverse [1..9]
    print $ isPalindrome [1,2,4,8,16,8,4,2,1]
    print $ myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    print $ compress "aaabbccccdeee"
    print $ pack "aaabbccccdeee"
    print $ encode "aaabbccccdeee"
    print $ encodeModified "aaabbccccdeee"
    print $ (decodeModified . encodeModified) "aaabbccccdeee"
    print $ encodeDirected "aaabbccccdeee"
    print $ dpli [1,2,3]
    print $ repli [1,2,3] 3
    print $ myDrop "abcdefghik" 3
    print $ split [1,2,3,4,5,6] 3
    print $ slice [1,2,3,4,5,6] 2 4
    print $ rotate [1,2,3,4,5,6] 2
    print $ rotate [1,2,3,4,5,6] (-2)
