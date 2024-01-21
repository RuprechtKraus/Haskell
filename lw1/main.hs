main :: IO ()
main = do
    -- First Task
    putStr "N1: "
    let v1 = ((1, 'a'), "abc")
    print (snd (fst v1))

    -- Second Task
    putStr "N2: "
    let v2_1 = ['a', 'b', 'c']
    print (head (tail v2_1))

    putStr "N2: "
    let v2_2 = [['a', 'b'], ['c','d']]
    print (head (tail (head v2_2)))

    putStr "N2: "
    let v2_3 = [['a', 'c', 'd'], ['a', 'b']]
    print (head (tail (head (tail v2_3))))

    putStr "N2: "
    let v2_4 = [['a','d'], ['b', 'c']]
    print (head (head (tail v2_4)))

    -- Third Task
    putStr "N3: "
    print (take 20 [x | x <- [1..], odd x])

    putStr "N3: "
    print (take 20 [1,3..])

    putStr "N3: "
    print (take 20 (filter odd [1..]))

    -- Fourth Task
    putStr "N4: "
    print (map triangularNumber [1..50])

    -- Fifth Task
    putStr "N5: "
    print (map squarePyramidalNumber [1..50])

triangularNumber 1 = 1
triangularNumber n = triangularNumber (n - 1) + n

squarePyramidalNumber 1 = 1
squarePyramidalNumber n = floor ((2 * n ^ 3 + 3 * n ^ 2 + n) / 6)