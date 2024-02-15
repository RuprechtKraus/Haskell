import System.Directory.Internal.Prelude (getArgs)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Text
import Data.Char (digitToInt, isSpace)
import System.Directory (removeFile)
import Data.ByteString (isPrefixOf)

fileActions = [
    ("View", view),
    ("Add", add),
    ("Delete", delete),
    ("Copy", copy)]

showHelp :: IO ()
getUserChoice :: IO Int

view :: [FilePath] -> IO ()
add :: [FilePath] -> IO ()
delete :: [FilePath] -> IO ()
copy :: [FilePath] -> IO ()

copyWithoutSpaces :: FilePath -> FilePath -> IO ()
copyWithoutComments :: FilePath -> FilePath -> IO ()

getUserInputAsInt :: IO Int
deleteAt :: Int -> [a] -> [a]

main = do
    args <- getArgs
    if length args /= 1
        then error "Expected one argument: <input file>"
        else do
            showHelp
            putStrLn ""

            selection <- getUserChoice
            putStrLn ""

            let action = snd $ fileActions !! (selection - 1)
            action args

            return ()

showHelp = do
    putStrLn "Available actions: "
    mapM_ (\(n, x) -> print $ show n ++ " - " ++ x) $ zip [1..] [fst x | x <- fileActions]

getUserChoice = do
    putStrLn "Choose an action: "
    getUserInputAsInt

view [filePath] = do
    putStrLn "File content: "
    contents <- readFile filePath
    putStr contents

add [filePath] = do
    putStrLn "Enter line to append to file: "
    line <- getLine
    appendFile filePath line

delete [filePath] = do
    putStrLn "Number of line to delete: "
    lineNumberStr <- getLine
    let lineNumber :: Int = read lineNumberStr

    -- Используем модуль Text для того чтобы избежать проблем с ленивым чтением файла из-за которого перезапись еще открытого файла невозможна
    contents <- Text.readFile filePath 
    let allLines = Text.lines contents

    Text.writeFile filePath $ Text.unlines $ deleteAt lineNumber allLines

copy [filePath] = do
    putStrLn "Select filtration method:\n1 - No spaces\n2 - No comments"
    selection <- getUserInputAsInt

    putStrLn "Enter output file path: "
    outputFilePath <- getLine
    
    case selection of
        1 -> copyWithoutSpaces filePath outputFilePath
        2 -> copyWithoutComments filePath outputFilePath
        _ -> putStrLn "Not supported choice"
    
copyWithoutSpaces inputFilePath outputFilePath = do
    contents <- Text.readFile inputFilePath
    Text.writeFile outputFilePath $ Text.filter (not . isSpace) contents

copyWithoutComments inputFilePath outputFilePath = do
    contents <- Text.readFile inputFilePath
    let commentPrefix =  Text.pack "#"
    let filteredLines = filter (not . Text.isPrefixOf commentPrefix) (Text.lines contents)
    Text.writeFile outputFilePath $ Text.unlines filteredLines

getUserInputAsInt = read <$> getLine
deleteAt n xs = take (n - 1) xs ++ drop n xs