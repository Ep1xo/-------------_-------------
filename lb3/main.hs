-- Завдання 1. Незмінне оновлення елементів списку (Haskell)
updateAt :: Int -> a -> [a] -> [a]
updateAt _ _ [] = []
updateAt i val (x:xs)
  | i < 0     = x : xs
  | i == 0    = val : xs
  | otherwise = x : updateAt (i - 1) val xs

-- Завдання 4. Створення оновленої копії запису (Haskell)
data Person = Person { name :: String, age :: Int } deriving Show

incrementAge :: Person -> Person
incrementAge p = p { age = age p + 1 }

-- Завдання 6. Видалення елемента з незмінного списку (Haskell)
removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt i (x:xs)
  | i < 0     = x : xs
  | i == 0    = xs
  | otherwise = x : removeAt (i - 1) xs


main :: IO ()
main = do
    print (updateAt 1 99 [10, 20, 30])
    print (updateAt 0 42 ([] :: [Int]))
    print (updateAt 5 7 [1,2,3])

    let p = Person "Anna" 25
    print (incrementAge p)
    print p

    print (push 4 [3,2,1])
    print (push 10 [])

    print (removeAt 1 [10, 20, 30])
    print (removeAt 5 [1, 2, 3])
