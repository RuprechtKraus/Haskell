doMyList :: Int -> [Int]
doMyList n = take n [n..]

oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven (x:y:xs) = y:x:oddEven xs

insert :: [a] -> a -> Int -> [a]
insert l a n = take n l ++ [a] ++ drop (n + 1) l

listSum :: Num c => [c] -> [c] -> [c]
listSum l1 l2 = zipWith (+) (extend l1 len) (extend l2 len)
  where len = max (length l1) (length l2)
        extend lst n = lst ++ replicate (n - length lst) 0

position :: (Num a, Eq t) => [t] -> t -> a
position [] _ = 0
position (x:xs) a = if x /= a then position xs a + 1 else 0

gaussSum1 :: (RealFrac a, Integral b) => a -> b
gaussSum1 n = floor ((n * (n + 1)) / 2)

gaussSum2 :: (RealFrac a, Integral b) => a -> b
gaussSum2 n = floor ((n - 1) * n / 2)

main = do
    let list = doMyList 6

    putStr "N1_output: "
    print (oddEven list)

    putStr "N2_output: "
    print (insert list 999 3)

    putStr "N3_output: "
    print (listSum [1, 2, 3] [5])

    putStr "N4_output: "
    print (position [1, 2, 3, 4, 5] 4)

    putStr "N5_output: "
    print (gaussSum1 50)

    putStr "N6_output: "
    print (gaussSum2 50)