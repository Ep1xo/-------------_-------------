-- Завдання 1. Подвоєння парних чисел
doubleEven :: [Int] -> [Int]
doubleEven xs = map (\x -> if even x then 2 * x else x) xs

-- Завдання 2. Сума непарних чисел
sumOdd :: [Int] -> Int
sumOdd xs = sum [x | x <- xs, odd x]

-- Завдання 3. Абсолютні значення
absList :: [Int] -> [Int]
absList xs = map abs xs

-- Завдання 4. Рекурсивна довжина списку
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Завдання 5. Рекурсивне перевертання списку
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Завдання 6. Максимум списку
myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "myMaximum: empty list"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

-- Завдання 7. Піфагорові трійки
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples =
  [ (a, b, c)
  | a <- [1..20]
  , b <- [1..20]
  , c <- [1..20]
  , a * a + b * b == c * c
  ]

-- Завдання 8. Фібоначчі
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
  | n > 1 = fib (n - 1) + fib (n - 2)
  | otherwise = error "fib: n must be non-negative"

-- ГОЛОВНА ФУНКЦІЯ ДЛЯ JDoodle
main :: IO ()
main = do
    print (doubleEven [1,2,3,4])
    print (sumOdd [1,2,3,4,5])
    print (absList [-3,5,-7])
    print (myLength [1,2,3,4])
    print (myReverse [1,2,3])
    print (myMaximum [3,7,2,9,4])
    print pythagoreanTriples
    print (fib 10)