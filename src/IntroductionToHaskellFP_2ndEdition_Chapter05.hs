module IntroductionToHaskellFP_2ndEdition_Chapter05 (test5) where

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "n is less than 0"
  | n==0 = 1
  | otherwise = n * factorial(n-1)

mygcd :: Int -> Int -> Int
mygcd x y = if y == 0 then x else mygcd y (mod x y)

-- 5.1 递归函数的概念
test5_1 :: IO()
test5_1 = do
  putStrLn "-----[ test5_1 ]-----"
  print (factorial 3) -- 6
  print (mygcd 12 8) -- 4

power :: Int -> Int -> Int
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n - 1)

{- 上面这样一步一步计算是很慢的 -}
power' :: Int -> Int -> Int
power' 0 0 = 1
power' _ 0 = 1
power' x n | odd n = let p = power' x ((n-1) `div` 2) in x * p * p
           | otherwise = let p = power' x (n `div` 2) in p * p

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y: snoc x ys

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

-- 5.2 简单递归函数
test5_2 :: IO()
test5_2 = do
  putStrLn "-----[ test5_2 ]-----"
  print(power 10 4)
  print(power' 10 4)
  print(product' [1,2,3])
  let cons = (:)
  print (cons 5 [1,2,3]) -- [5,1,2,3]
  print (snoc 4 [1,2,3]) -- [1,2,3,4]
  print (last' [1,2,3])
  print (elem' 2 [1,2,3])

-- 5 递归函数
test5 :: IO()
test5 = do
  test5_1
  test5_2