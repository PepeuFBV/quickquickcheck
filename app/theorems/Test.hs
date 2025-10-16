module Test
  ( plusOne, increment, double, addCommutative, addAssociative, multCommutative, identity, squareAlwaysPositive, wrongDouble,
  factorial, isEmpty, stringLength, reverseConcat, isEven, headSingleton, sortPreservesLength, quicksort
  )
where

-- @quickcheck plusOne(x) == x + 1
-- @quickcheck plusOne(0) == 1
-- @quickcheck plusOne(1) == 2
plusOne :: Integer -> Integer
plusOne x = x + 1

-- @quickcheck increment(x) > x
increment :: Int -> Int
increment x = x + 1

-- @quickcheck double(x) == x + x
double :: Int -> Int
double x = x * 2

-- @quickcheck addCommutative(x, y) == addCommutative(y, x)
addCommutative :: Int -> Int -> Int
addCommutative x y = x + y

-- @quickcheck addAssociative(x, y, z) == addAssociative(y, z, x)
addAssociative :: Int -> Int -> Int -> Int
addAssociative x y z = x + (y + z)

-- @quickcheck multCommutative(x, y) == y * x
multCommutative :: Int -> Int -> Int
multCommutative x y = x * y

-- @quickcheck identity(x) == x
identity :: Int -> Int
identity x = x + 0

-- @quickcheck squareAlwaysPositive(x) >= 0
squareAlwaysPositive :: Int -> Int
squareAlwaysPositive x = x * x

-- @quickcheck wrongDouble(x) == x + x
wrongDouble :: Int -> Int
wrongDouble x = x + x + 1

-- @quickcheck factorial(0) == 1
-- @quickcheck factorial(5) == 120
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- @quickcheck isEmpty([]) == True
isEmpty :: [Int] -> Bool
isEmpty = null

-- @quickcheck stringLength(s) == length s
stringLength :: String -> Int
stringLength s = length s

-- @quickcheck reverseConcat(xs, ys) == reverse ys ++ reverse xs
reverseConcat :: [Int] -> [Int] -> [Int]
reverseConcat xs ys = reverse (xs ++ ys)

-- @quickcheck isEven(x) == (x `mod` 2 == 0)
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- @quickcheck headSingleton(x) == x
headSingleton :: Int -> Int
headSingleton x = head [x]

-- @quickcheck sortPreservesLength(xs) == length xs
sortPreservesLength :: [Int] -> Int
sortPreservesLength xs = length (quicksort xs)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                 ++ [x]
                 ++ quicksort [y | y <- xs, y > x]

-- TODO: create more tests