{-# LANGUAGE BinaryLiterals #-}
module IntroductionToHaskellFP_2ndEdition_Chapter02 (test2) where

import Data.Complex
import Data.Ratio
import Data.Fixed

-- 2.1.1 Haskell 常用数据类型
test2_1_1 :: IO ()
test2_1_1 = do
  putStrLn "-----[ test2_1_1 ]-----"
  print (True || False) -- True
  print (not True) -- False
  print ('\100') -- 'd'
  print ('你') -- '\20320'
  putStrLn "\\" -- \
  putStrLn "abc\&def" -- abcdef
  putStrLn "hello\tworld" -- hello   world
  print (2 ^ 32 :: Int) -- 0
  {- Word 类型是无符号的整数，它的范围也是系统相关的。在 32 位系统中它的范围是 0 ∼ 2 ^ 32 − 1 而 64 位系统则为 0 ∼ 2 ^ 64 − 1。
     Haskell 中的 Word 相当于 C 语言里的 unsigned int 类型。-}
  print (-1 :: Word) -- 4294967295
  {- 任意精度整数：Integer -}
  print (2 ^ 32 :: Integer) -- 4294967296
  {- 八进制 -}
  print (0o12) -- 10
  {- 十六进制 -}
  print (0x3a) -- 58
  {- 二进制
     但是用二进制表示整数需要 GHC 7.10 以上版本的 BinaryLiterals 语言扩展。
     如果需要在代码中使用二进制数值就需要在文件首处加上 {-# LANGUAGE BinaryLiterals #-}，这就是语法标准以外的特性。-}
  print (0b11111111) -- 255
  print (pi :: Float) -- 3.1415927
  print (pi :: Double) -- 3.141592653589793
  {- Haskell 还有有理数类型 Rational，即用两个任意精度的整数来表示一个小数，这在做高精度数学运算时有很多好处。
     % 相当于分数线，2500 为分母，10333 为分子，这样，一个小数就被转换为了分数。-}
  print (4.1332 :: Rational) -- 10333 % 2500
  print (['H', 'e', 'l', 'l', 'o']) -- "Hello"
  {- Haskell 中最多支持 62 个元素的元组。 -}
  let tuple = (5, True)
  print (fst tuple) -- 5
  print (snd tuple) -- True

-- 2.1.2 函数类型
test2_1_2 :: IO ()
test2_1_2 = do
  putStrLn "-----[ test2_1_2 ]-----"
  let add (x, y) = (x + y) :: Int
  print (add(1, 2))
  let add' x y = x + y :: Int
  print (add' 4 5)
  print (curry add 4 5)
  print (uncurry add' (1, 2))
  {- 多态函数 -}
  print (fst (1, True))
  print (fst ([1, 2, 3], False))
  print (length [1, 2, 3, 4])
  print (length [True, False, False])
  print (head [1, 2, 3])
  print (head [True, False])
  print (head "Hello")
  print (zip [1, 2, 3] [4, 5, 6])
  print (zip "abc" [1, 2, 3])

-- 2.2.1 相等类型类：Eq
test2_2_1 :: IO()
test2_2_1 = do
  putStrLn "-----[ test2_2_1 ]-----"
  print (5 /= 4)

-- 2.2.2 有序类型类：Ord
test2_2_2 :: IO()
test2_2_2 = do
  putStrLn "-----[ test2_2_2 ]-----"
  print ("Hello" < "World")
  print ([1, 2, 3] < [5, 6, 7])

-- 2.2.3 枚举类型类：Enum
test2_2_3 :: IO()
test2_2_3 = do
  putStrLn "-----[ test2_2_3 ]-----"
  print ([1..10])
  print (['a'..'d'])
  print (succ 1)
  print (pred 'M')

-- 2.2.4 有界类型类：Bounded
test2_2_4 :: IO()
test2_2_4 = do
  putStrLn "-----[ test2_2_4 ]-----"
  print (maxBound :: Bool)
  print (minBound :: Bool)
  print (maxBound :: Int)
  print (minBound :: Int)
  print (maxBound :: Char)

-- 2.2.5 数类型类：Num
test2_2_5 :: IO()
test2_2_5 = do
  putStrLn "-----[ test2_2_5 ]-----"
  print (truncate (4.6 :: Float))
  -- 复数
  print ((5 :+ 5) + (1 :+ 1))
  print (sin (5 :+ 5))
  -- Float 和 Double 两个小数类型实际是 Enum 类型类，可以以固定的差值来遍历
  print ([1.0, 1.5 .. 3.0])
  -- succ 与 pred 默认的长度是 1
  print (pred 0.1) -- -0.9
  print (fromInteger 5 :: Complex Double)
  print (fromInteger 5 :: Ratio Int)
  print (fromRational (5 % 1))
  print (1/3)
  print (1/3 :: Fixed E6)
  print (1/3 :: Micro) -- 效果同上
  print (div' 1.7 0.3) -- 5
  print (mod' 7.3 1.2) -- 0.10000000000000053 精度错误
  print (mod' 7.3 1.2 :: Nano) -- 0.100000000 手动限制精度
  print (0 / 0)
  print (0 / 0 == 0 / 0)
  print (-1 / 0)
  print (1 / 0)
  print (isInfinite(1 / 0))

-- 2 类型系统和函数
test2 :: IO ()
test2 = do
  test2_1_1
  test2_1_2
  test2_2_1
  test2_2_2
  test2_2_3
  test2_2_4
  test2_2_5