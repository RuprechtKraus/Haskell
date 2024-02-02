import Data.List (nub)

listnums :: (Ord t, Num t) => t -> [t]
listnums n
    | n <= 0 = []
    | otherwise = n : listnums (n - 1)

secondlastlist :: [[a]] -> [a]
secondlastlist (xs:xss)
    | null xss = [last xs]
    | otherwise = last xs : secondlastlist xss

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] [] = []

myunion (x:xs) []
    | x `elem` xs = myunion [] xs
    | otherwise = x : myunion [] xs

myunion [] (y:ys)
    | y `elem` ys = myunion [] ys
    | otherwise = y : myunion [] ys

myunion (x:xs) ys
    | x `elem` xs || x `elem` ys = myunion xs ys
    | otherwise = x : myunion xs ys


mysubst [] _ = []
mysubst xs [] = xs
mysubst (x:xs) ys
    | x `notElem` xs && x `notElem` ys = x : mysubst xs ys
    | otherwise = mysubst xs ys

nthElems :: [[b]] -> Int -> [b]
nthElems xss n = map (!! n) xss 

main :: IO ()
main = do
    putStr "task 1: "
    print $ listnums 10

    putStr "task 2: "
    print $ secondlastlist [[1], [2]]

    putStr "task 3: "
    print $ myunion [1, 2, 3] [2, 3, 3, 4]

    putStr "task 4: "
    print $ mysubst [1, 2, 3, 3, 4, 5] [2, 4]

    putStr "task 5: "
    print $ nthElems [[1, 2, 3], [1, 2, 3], [1, 2, 3]] 1