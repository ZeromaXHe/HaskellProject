{-# LANGUAGE BinaryLiterals #-}
module IntroductionToHaskellFP_2ndEdition_Chapter02 where

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