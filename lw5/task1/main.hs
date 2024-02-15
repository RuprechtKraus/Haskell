import Data.List

main = do
    putStrLn "Введите количество элементов: "
    count <- readLn
    putStrLn "Введите начальное значение: "
    start <- readLn
    putStrLn "Введите кратность: "
    step <- readLn
    let (Just first) = find (\x -> x `mod` step == 0) [start..]
    let numbers = take count [first, first + step ..]
    print numbers