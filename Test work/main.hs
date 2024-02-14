import Data.List
import Control.Monad (forM)
import Data.Function (on)

partitionN :: [a] -> Int -> [[a]]
partitionN _ n | n <= 0 = error "List number must be non negative integer"
partitionN [] _ = []
partitionN xs n =
    let totalLength = length xs
        sublistLength = (totalLength + n - 1) `div` n -- Длину подсписков округляем в большую сторону, чтобы распределить элементы более равномерно
        partitionHelper [] _ = []
        partitionHelper ys count
            | count == n = [ys] -- Оставшиеся элементы формируют последний подсписок
            | otherwise = take sublistLength ys : partitionHelper (drop sublistLength ys) (count + 1)
    in partitionHelper xs 0

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x xs = [i | (y, i) <- zip xs [0..], x == y]

generatePowers :: Int -> Int -> [[Int]]
generatePowers maxNum count = [take count [x^y | y <- [1..]] | x <- [1..maxNum]]

countWords :: FilePath -> FilePath -> IO ()
countWords inputFile outputFile = do
    contents <- readFile inputFile
    let groupedWords = group $ sort $ words contents -- Группируем слова в подписки, предварительно отсортировав список слов
    let wordCounts = [(head x, length x) | x <- groupedWords] -- Формируем массив пар, где первый элемент - слово, второй - его как часто он встречается в тексте
    let wordCountsDesc = sortBy (flip compare `on` snd) wordCounts -- Сортируем все по убыванию частоты встречаемости
    allLines <- forM wordCountsDesc (\(word, frequency) -> do
        let line = word ++ ":" ++ show frequency
        return line)
    writeFile outputFile $ unlines allLines

main = do
    -- Task 1
    print $ partitionN [1,2,3,4,5,6,7] 3

    -- Task 2
    print $ elemIndices' 2 [1,2,3,1,2,3]

    -- Task 3
    print $ generatePowers 4 2

    -- Task 4
    countWords "input.txt" "output.txt"