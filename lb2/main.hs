-- Завдання 1. Безпечне отримання першого елемента
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Завдання 2. Безпечне ділення
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-- Завдання 3. Побудова множника
mkMultiplier :: Num a => a -> (a -> a)
mkMultiplier n = (\x -> n * x)

-- Завдання 4. Застосування функції двічі
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Завдання 5. Власна реалізація map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Завдання 6. Фільтрація за предикатом
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs

-- Завдання 7. Обчислення середнього значення
avg :: (Fractional a) => [a] -> Maybe a
avg [] = Nothing
avg xs = Just (sum xs / fromIntegral (length xs))

-- Завдання 8. Композиція перетворень
processList :: [Int] -> [Int]
processList xs = take 3 (map (^2) (filter odd xs))

-- Завдання 9. Пошук елемента в списку
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex v xs = go xs 0
  where
    go [] _ = Nothing
    go (y:ys) i
      | y == v    = Just i
      | otherwise = go ys (i + 1)

-- Завдання 10. Узагальнене застосування списку функцій
applyAll :: [a -> a] -> a -> a
applyAll fs x = foldl (\acc f -> f acc) x fs

-- Завдання 11. Бінарний пошук
binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch xs target = go xs target 0
  where
    go [] _ _ = Nothing
    go ys t offset =
      let mid = length ys `div` 2
          midVal = ys !! mid
      in if t == midVal
         then Just (offset + mid)
         else if t < midVal
              then go (take mid ys) t offset
              else go (drop (mid + 1) ys) t (offset + mid + 1)

-- Завдання 12. Швидке сортування (quicksort)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smaller = quickSort [y | y <- xs, y <= x]
      bigger = quickSort [y | y <- xs, y > x]
  in smaller ++ [x] ++ bigger

-- Завдання 13. Ханойські вежі
type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to temp =
  hanoi (n-1) from temp to ++
  [(from, to)] ++
  hanoi (n-1) temp to from

--
main :: IO ()
main = do
    print (safeHead [10,20,30])
    print (safeHead ([] :: [Int]))
    print (safeDiv 10 2)
    print (safeDiv 7 0)
    print (mkMultiplier 2 5)
    print (applyTwice (+1) 3)
    print (myMap (*2) [1,2,3])
    print (myFilter even [1..10])
    print (avg [1,2,3,4])
    print (processList [1..10])
    print (findIndex 3 [1,2,3,4])
    print (applyAll [(*2), (+1), (^2)] 3)
    print (binarySearch [1,3,5,7,9] 5)
    print (quickSort [3,1,4,1,5,9,2])
    print (hanoi 2 "A" "C" "B")

--Що таке замикання функції?
--Замикання функції — це функція, яка запам’ятовує значення змінних із зовнішнього оточення, у якому була створена, і може використовувати їх пізніше.

--Що таке функції вищого порядку?
--Функції вищого порядку — це функції, які приймають інші функції як аргументи або повертають функцію як результат.

--Для чого використовується рекурсія у функціональному програмуванні?
--Рекурсія у функціональному програмуванні використовується для реалізації повторюваних обчислень без використання циклів, оскільки дозволяє природно обробляти рекурсивні структури даних, такі як списки та дерева.
