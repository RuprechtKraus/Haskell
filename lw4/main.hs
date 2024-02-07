import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse sep (x:xs) = x : sep : myIntersperse sep xs

myIsAlphaNum :: Char -> Bool
myIsAlphaNum c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices elem list = [i | (x, i) <- zip list [0..], x == elem]

main = do
    print $ myIntersperse ',' "abcde"  -- "a,b,c,d,e"
    print $ myIntersperse 0 [1,2,3,4]  -- [1,0,2,0,3,0,4]
    print $ myIntersperse 100 []       -- []

    print $ myIsAlphaNum 'a' -- True
    print $ myIsAlphaNum '1' -- True
    print $ myIsAlphaNum '?' -- False

    print $ Map.size $ Map.fromList [(1, "a"), (2, "b"), (3, "c")]  -- 3
    print $ Map.size Map.empty                                      -- 0

    let setA = Set.fromList [1, 2, 3]
    let setB = Set.fromList [2, 3, 4]
    print $ Set.difference setA setB  -- fromList [1]
    print $ Set.difference setB setA  -- fromList [4]

    print $ myElemIndices 'a' "banana"              -- [1,3,5]
    print $ myElemIndices 5 [1,2,3,4,5,6,5,4,3,2,1] -- [4,6]
    print $ myElemIndices 10 [1,2,3]                -- []

    print $ elemIndices 'b' (concatMap (replicate 3) "abcabc")