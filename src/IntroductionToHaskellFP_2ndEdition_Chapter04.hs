module IntroductionToHaskellFP_2ndEdition_Chapter04 (test4) where

import Data.Char
import Data.List
import Data.Bits

{- 未定义 undefined 函数是由 error 函数定义的
undefined :: a 2
undefined = error "Prelude; undefined"
由于 undefined 是一个多态类型 a，因此可以用其暂时定义没有实现的函数，这是为了保证调试时 GHCi 不会报错。 -}
month :: Int -> Int
month = undefined

days :: (Int, Int) -> Int
days (m, d) = month m + d

-- 4.1.1 常用函数
test4_1_1 :: IO()
test4_1_1 = do
  putStrLn "-----[ test4_1_1 ]-----"
  {- 恒值函数 id :: a -> a，它是给定一个任何的值，都返回这个给定的值的函数。 -}
  print (id 5)
  {- 常值函数 const 函数 const :: a -> b -> a 的作用是给定两个元素，只返回第一个 -}
  print (const True 3) -- True
  {- 之前讨论的恒值函数 id 虽然简单，但其实是一个很常用的函数，常常用它来“占位”。
  比如，当需要定义一个给定两个参数，且只返回第二个参数的函数就可以定义为 const id -}
  print (const id True 3) -- 3
  {- const id True 3
   = (const id True) 3 {函数应用为左结合}
   = id 3 {应用 const 函数，将 id 函数返回}
   = 3 {应用 id 函数} -}
  {- 函数 flip :: (a -> b -> c) -> b -> a -> c 可以将一个二元函数的两个参数的顺序颠倒，以后会了解到这个函数的用处。-}
  print (flip (-) 3 8) -- 5
  print (flip (:) [1,2,4] 5) -- [5,1,2,4]
  print (flip const True 3) -- 3
  {- 异常函数 error :: String -> a 是抛出异常的函数。有些时候，程序中出现的错误会导致整个程序停止运行。
  这时，就可以用 error 函数来返回错误信息。返回时，使用一个字符串来告诉用户为什么有错误。
  a = error "a is an error" -}
  {- 这是一对常用的函数，min :: Ord a => a -> a -> a 将返回两个参数中较小的那一个，而 max 则会返回两个参数中较大的那一个。-}
  print (min 5 6)
  print (max 5 6)

{- 类型有问题，不知道咋定义 -}
--avg :: [Int] -> Int
--avg xs = sum xs / (fromIntegral $ length xs)
--avg' :: [Int] -> Int
--avg' xs = sum xs / genericLength xs

-- 4.1.2 基于列表的函数
test4_1_2 :: IO()
test4_1_2 = do
  putStrLn "-----[ test4_1_2 ]-----"
  {- null 函数会判定一个列表是否为空，它的类型读者可以暂时认为是 [a] -> Bool。[a] -> Bool 是 GHC 7.8 的类型，
  实际上它可以应用到更多容器类型上，比如 Data.Array 中的数组，Data.Tree 中的树等等，
  所以在 GHC 7.10 后它的类型是 Foldable t => t a -> Int，关于 Foldable 类型类我们会在第9.4.4章讨论。 -}
  print (null []) -- True
  print (null [1,3]) -- False
  {- length 函数会返回列表的长度，可暂时把它的类型看作是 [a] -> Int，返回的长度的类型不是任意精度的整数而是 Int。
  因此，如果处理过长的列表，就不可以用这个函数了。此时，可以使用 Data.List 库中的 genericLength 函数。-}
  print (length [1,3,4,5,8]) -- 5
--  print (avg [1,3,5])
--  print (avg' [1,3,5])
  {- 之前已经介绍过 (!!) 函数。这个运算符可以取得给定列表中从 0 开始的第 n 个元素。这个运算符的类型是 [a] -> Int -> a -}
  print ([1,2,3,4]!!0) -- 1
  print ([True,False,True]!!2) -- True
  {- reverse 倒置函数可以将列表中元素的顺序倒过来 -}
  print (reverse [1,2,3]) -- [3,2,1]
  {- head 和 last 这两个函数分别取一个列表的第一个元素与最后一个元素。它们的类型都是 [a] -> a。如果列表为空，则会报出错误。-}
  print (head "Hello World!") -- 'H'
  print (last "Hello World!") -- '!'
  {- init 和 tail 它们分别将一个列表的最后一个元素与第一个元素去掉，得到一个新的列表。它们的类型是 [a] -> [a]。如果列表为空，则会报出错误。-}
  print (init [1..10]) -- [1,2,3,4,5,6,7,8,9]
  print (tail [1,3..15]) -- [3,5,7,9,11,13,15]
  {- map 意为映射，会将一个函数应用到列表中的每一个元素，然后得一个新的列表。它的类型是 (a -> b) -> [a] -> [b]。-}
  print (map (+1) [2,3,4,5]) -- [3,4,5,6]
  print (map not [True,False,False]) -- [False, True, True]
  print (map (\x -> x^2 + 1) [2,3,4,5]) -- [5,10,17,26]
  {- filter 是过滤函数，需要一个条件判断的函数为参数，使得可以从一个列表中选出满足给定条件的元素。
  为了使用 filter，这里再介绍两个很简单的函数，即 even 与 odd。给定一个整数，通过这两个函数可以判断其奇偶性。-}
  print (odd 4) -- False
  print (even 4) -- True
  print (filter even [1,2,3,4,5,6]) -- [2,4,6]
  print (filter (>=7) [9,6,4,2,10,3,15]) -- [9,10,15]
  {- take 函数可以从头连续地取得一个列表的几个元素。它们的类型是 Int -> [a] -> [a]。如果这个数为负，则结果为空。
  如果要取的个数比总长度要大，则全部取下。drop 函数与 take 函数相反，是将列表中前几个元素舍弃。-}
  print (take 5 [1..]) -- [1,2,3,4,5]
  print (drop 3 [1,2,3,4,5,6]) -- [4,5,6]
  {- span 和 break 这两个函数的类型都是 (a -> Bool) -> [a] -> ([a], [a])。
  span 函数可以根据一个条件，从左至右，当遇到第一个不符合条件的元素时停止，将一个列表分成由两个列表组成的元组。
  break 函数则与 span 函数相反，它会根据一个条件，从左至右，当遇到符合条件的时候停止。-}
  print (span even [2,4,6,7,8,9]) -- ([2,4,6],[7,8,9])
  print (break odd [2,4,6,7,8,9])
  {- 之前的 take 和 drop 函数是通过给定一个整数来取得或者去掉列表中的前几个元素，
  而 takeWhile 和 dropWhile 则需要一个条件来判断，条件不成立的时候停止取出或者去除。
  实质上 takeWhile 取的是 span 结果的第一个列表，dropWhile 取的是 span 结果的第二个列表。-}
  print (takeWhile (>5) [6,7,8,3,5,9]) -- [6,7,8]
  {- splitAt 这个函数可以将一个列表在任何的位置分开。它的类型是 Int -> [a] -> ([a], [a]) 。-}
  print (splitAt 5 "Hello World!") -- ("Hello", " World!")
  {- 重复函数 repeat 可以将一个元素在列表里重复无数次。
  repeat True 这是一个无穷列表，点击 WinGHCi 的停止按钮或者按 Ctrl+C 组合键来终止程序。也可以用 [True..] 来表达这个无穷的列表。
  replicate 是复制函数，它的类型是 Int -> a -> [a]。这个函数可以将一个元素复制给定的次数 -}
  print (take 5 (repeat True))
  print (replicate 5 True) -- [True, True, True, True, True]
  {- any 和 all 这两个函数的类型可暂时认为是 (a -> Bool) -> [a] -> Bool。
  any 可以查询一个列表中是否存在符合给定条件的元素，而 all 会判定列表中是否所有的元素都符合给定条件。-}
  print (any even [1,2,3,4]) -- True
  print (all odd [1,3,5]) -- True
  {- and 和 or 它们的类型可以暂认为 [Bool] -> Bool，and 会把一个列表中所有的布尔值用 && 连接起来，如果是空列表那么返回 True；
  而 or 则会把所有布尔值用 || 连接起来，如果列表为空则返回 False。-}
  print (or []) -- False
  print (and [True, True, False]) -- False
  {- 函数 elem 与 notElem 的类型可以暂时认为是 Eq a => a -> [a] -> Bool，elem 函数可以判断一个列表中是否存在某一元素。
  显然，a 一定要是可以比较相等的类型，所以在类型签名中需要 Eq 类型类。同样，prelude 中还有 notElem，notElem 是 elem 的否定。-}
  print (elem 1 [4,5,1]) -- True
  {- 迭代函数 iterate 的类型为 (a -> a) -> a -> [a]，它可以将第一个参数中的函数应用在第二个参数上，并重复应用到函数结果上。-}
  print (take 10 (iterate (*2) 1))
  {- 函数 until :: (a -> Bool) -> (a -> a) -> a -> a 可以迭代地来生成数据直到满足给定的条件为止。
  它需要一个停止条件、一个迭代的函数还有一个迭代的初始值，首次到达条件时停止，然后返回结果。-}
  print (until (>500) (*2) 1) -- 512
  {- zip 函数可以将两个列表结合成一个元组的列表。当元素个数不相等的时候，多余的元素会被忽略。-}
  print (zip [True, False, True, False] [2,4,5,6,7]) -- [(True,2),(False,4),(True,5),(False,6)]
  print (zip "Hello World" [0..]) -- [('H',0),('e',1),('l',2),('l',3),('o',4),(' ',5),('W',6),('o',7),('r',8),('l',9),('d',10)]
  {- unzip 是把一个二元元素列表分成两个列表元素的函数，unzip :: [(a, b)] -> ([a], [b])，即将 zip 后的列表还原 -}
  print (unzip [('H',0),('e',1),('l',2),('l',3),('o',4),(' ',5),('W',6),('o',7),('r',8),('l',9),('d',10)]) -- ("Hello World",[0,1,2,3,4,5,6,7,8,9,10])
  {- zipWith 函数的类型是 (a -> b -> c) -> [a] -> [b] -> [c]，相信读者只要看类型就会知道这个函数可以做什么。
  它需要一个二元函数作为参数，然后再输入两个列表，最后把两个列表中的元素取出一一对应地进行第一个参数指定的二元运算。-}
  print (zipWith (+) [5,6,7,3] [2,3,4,7]) -- [7,9,11,10]
  {- 当然，有的时候要用到将三个列表的中元素合成有三个元件的元组，这时可以用到预加载库中的 zip3、unzip3。
  此外，Prelude 中还提供 zipWith3 等函数，读者可以自己查阅一下 API，然后试用一下。-}
  {- concat 函数可以将一个列表中的列表相连，暂且认为它的类型是 [[a]] -> [a]。-}
  print (concat [[1,2],[3,4]]) -- [1,2,3,4]
  {- concatMap 它的类型可以暂时为 (a -> [b]) -> [a] -> [b]。
  这个函数先使用 map 函数将 [a] 计算为 [[b]] 类型的结果，再使用 concat 函数来得到类型为 [b] 的结果。-}
  print (map (replicate 3) [1,2,3]) -- [[1,1,1],[2,2,2],[3,3,3]]
  print (concatMap (replicate 3) [1,2,3]) -- [1,1,1,2,2,2,3,3,3]

type Weekday = Int
type Year = Int
type Month = Int
type Day = Int

week' :: Year -> Day -> Weekday
week' y d = let y1 = y - 1 in (y1 + (div y1 4) - (div y1 100) + (div y1 400) + d) `mod` 7

isLeapYear :: Int -> Bool
isLeapYear y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

monthDays :: Year -> Month -> Int
monthDays y m | m == 2 = if not $ isLeapYear y then 28 else 29
              | elem m [1,3,5,7,8,10,12] = 31
              | elem m [4,6,9,11] = 30
              | otherwise = error "invalid month"

accDays :: Year -> Month -> Day -> Int
accDays y m d | d > monthDays y m = error "invalid days"
              | otherwise = (sum $ take (m-1) (map (monthDays y) [1..12]))+d

week :: Year -> Month -> Day -> Weekday
week y m d = week' y (accDays y m d)

-- 4.2 定义历法公式
test4_2 :: IO()
test4_2 = do
  putStrLn "-----[ test4_2 ]-----"
  print (week 2022 7 3) -- 0 （对应星期日）
  print (week 2022 7 7) -- 4 （对应星期四）

contains6 :: [String]
contains6 = filter (elem '6') (map show [1..100])

contains6' :: [Int]
contains6' = map (\str -> read str::Int) $ filter (elem '6') (map show [1..100])

-- 4.3 字符串处理的函数
test4_3 :: IO()
test4_3 = do
  putStrLn "-----[ test4_3 ]-----"
  {- show 函数的类型 Show a => a -> String，即可以将所有在 Show 类里的数据作为字符串，String 类型输出。注意当我们显示一个字符串时会有\转义-}
  print (show 6)
  print contains6
  print (show "abc") -- "\"abc\""
  {- read 函数类型 read :: Read a => String -> a 与 show 相反，可以将可读的类型从字符串 String 类型解析为类型 a。-}
  print contains6'
  {- lines 和 unlines lines 函数读入的字符串以'\n' 为分隔，变成一个 String 的列表，它的类型是 String -> [String]-}
  print (lines "first line\nsecond line") -- ["first line", "second line"]
  {- unlines 是 lines 的反函数，即以一个 [String] 作为输入，将列表内的字符串以 \n 换行符分开，所以它的类型是 [String] -> String。-}
  print (unlines ["first line", "second line"]) -- "first line\nsecond line"
  {- words 函数将一个字符串用空格分开，分成一个 String 的列表，words :: String -> [String]
   同样，unwords 是 words 的反函数。那么，unwords 的类型很明显是 [String] -> String。-}
  print (words "first second") -- ["first","second"]
  print (unwords $ reverse $ words "This is a sentence") -- "sentence a is This"

-- 4.4.1 Data.Char
test4_4_1 :: IO()
test4_4_1 = do
  putStrLn "-----[ test4_4_1 ]-----"
  {- 函数 chr 与 ord 可将 ASCII 码与字符相互转换。 -}
  print (chr 90) -- 'Z'
  print (ord 'Z') -- 90
  {- 此外，还有 isDigit、isLower、isUpper、isAlpha、isControl 等函数，分别为判定一个字符是不是数字、大写、小写、英文字母、控制字符。 -}
  print (isControl '\ESC') -- True

-- 4.4.2 Data.List
test4_4_2 :: IO()
test4_4_2 = do
  putStrLn "-----[ test4_4_2 ]-----"
  {- group :: Eq a => [a] -> [[a]] group 会把连续的相等元素放在一个列表中：-}
  print (group "Hello Miss") -- ["H","e","ll","o"," ","M","i","ss"]
  {- isPrefixOf, isSuffixOf, isInfixOf :: Eq a => [a] -> [a] -> Bool
   这 3 个函数的作用如它们的名字那样，第 1 个列表是不是第 2 个列表的前缀、后缀、中缀-}
  print (isPrefixOf "abc" ['a'..'z']) -- True
  print (isSuffixOf "xyz" ['a'..'z']) -- True
  print (isInfixOf ['d'..'h'] ['a'..'z']) -- True
  {- stripPrefix :: Eq a => [a] -> [a] -> Maybe [a] stripPrefix 会把给定的前缀去掉，如果不匹配那么就返回 Nothing -}
  print (stripPrefix "abc" "abcdef") -- Just "def"
  print (stripPrefix "123" "abcdef") -- Nothing
  {- findIndex :: (a -> Bool) -> [a] -> Maybe Int findIndex 会返回第 1 个满足条件的元素的位置-}
  print (findIndex even [1,2,3,4]) -- Just 1
  {- findIndices :: (a -> Bool) -> [a] -> [Int] findIndices 让我们找到一个列表中所有满足条件的值的位置。-}
  print (findIndices even [1,2,3,4,5]) -- [1,3]
  {- elemIndex :: Eq a => a -> [a] -> Maybe Int
  exemIndex 会返回第 1 个与给定值相等的元素的位置，其实定义起来就是 findIndex (x==)，这里就不多赘述了。-}
  {- elemIndices :: Eq a => a -> [a] -> [Int]
  elemIndices 这个函数会返回列表中所有的与给定值相等的元素的位置，可以使用 findIndices 定义为 findIndices (x==)，这里就不多赘述了。-}
  {- partition :: (a -> Bool) -> [a] -> ([a], [a]) partition 会把一个列表根据给定的条件分成两个 -}
  print (partition even [1,2,3,4,5,6]) -- ([2,4,6],[1,3,5])
  {- delete :: Eq a => a -> [a] -> [a] 从列表中删除第 1 个与给定元素相等的元素 -}
  print (delete 3 [1,2,2,3,3,3]) -- [1,2,2,3,3]
  {- deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] deleteBy 与只是可以给定条件的 delete，读者可以自己试验一下。-}
  {- Data.List 中还有很多函数，比如 union、intersect、sort、subsequences 等等，
  此外还有子模块 Data.List.Split，这里就不再列举了，读者可以参阅其 API 来了解。-}

-- 4 库函数及其应用
test4 :: IO()
test4 = do
  test4_1_1
  test4_1_2
  test4_2
  test4_3
  test4_4_1
  test4_4_2