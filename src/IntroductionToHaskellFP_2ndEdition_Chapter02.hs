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

{- a 就是一个全局的常量。在计算过程中，a 的值是不会改变的，用户只能引用值而不能修改值 -}
a :: Int
a = 5

-- 2.3.1 Haskell 中的值
test2_3_1 :: IO()
test2_3_1 = do
  putStrLn "-----[ test2_3_1 ]-----"
  print (2 * a)
  print (1 + a)

-- 2.3.2 函数思想入门
test2_3_2 :: IO()
test2_3_2 = do
  putStrLn "-----[ test2_3_2 ]-----"
  {- 这种写法只是将中缀运算符置前，（+）只是一个符号，也可以使用 add、plus 等用户想要的名字 -}
  print((+) 5 7)

add, sub :: Int -> Int -> Int
add a b = a + b
sub a b = a - b

{- 当有多个类型类限定在一个类型上的时候，它们需要用括号写在一起，并且中间要用逗号隔开，
或者像柯里化那样使用 => 依次连接，这并不是 Haskell 98 标准
function :: (Show a, Ord a) => a -> a -> a
function2 :: Show a => Ord a => a -> a -> a -}

{- 在数学中，常常将函数写成这样：f(x) = 4x + 1，其中 x 是一个数，需要被限定在 Num 类型类中。它在 Haskell 里被写成这样 -}
f :: Num a => a -> a
f x = 4 * x + 1

-- 2.3.3 函数的基本定义格式
test2_3_3 :: IO()
test2_3_3 = do
  putStrLn "-----[ test2_3_3 ]-----"
  {- 本节将讨论如何在 Haskell 中定义函数。一般情况下，先要在第一行定义函数名与函数的类型，它称为类型签名（type signature）。
  定义类型签名后，另起一行写出函数名、参数，最后定义函数体，在 Haskell 中定义的函数的大致格式是这样定义的：
  函数名 :: 参数 1 的类型 -> 参数 2 的类型 -> ... -> 结果类型
  函数名 参数 1 参数 2 ... = 函数体 -}
  print(add 1 2)
  {- 另外要注意的是，函数名不能以大写的英文字母和数字开头，因为大写开头的字母单词是被当成类型或者类型数据使用的，
  比如 Bool 类型是大写，并且 True 与 False 都以大写字母开头的类型的值。 -}
  print(f 5) -- 21

f'1 :: Num a => a -> a -> a
f'1 = \x -> \y -> 4*x + 5*y + 1
{- 参数依次输入的时候，在 Haskell 中可以省去
参数间的箭头而简写 -}
f'2 :: Num a => a -> a -> a
f'2 = \x y -> 4*x + 5*y + 1

-- 2.4 λ 表达式
test2_4 :: IO()
test2_4 = do
  putStrLn "-----[ test2_4 ]-----"
  {- Haskell 还有另外一种书写函数的格式，与函数的类型相互对应，这就是 λ 表达式。
  函数类型 :: 参数 1 的类型 -> 参数 2 的类型 -> ... -> 结果的类型
  函数名 = \参数 1 -> \参数 2 -> ... -> 函数体
  λ 函数的写法源于 λ 演算，很多关于函数式编程的书中将这里的反斜杠 \ 写成希腊字母 λ，
  有些语言里定义函数写成 f = λx → λy → x + y 或者 f = λx.λy.(x + y)，
  不过各个写法表达的都是同样的意思，就是将函数的参数依按次序输入到函数体中。
  这里输入的 x 与 y 不一定是值，也可以是函数，如 g = λx.λy.xy 是把 y 应用于函数 x，例如： -}
  print ((\x -> \y -> x y) abs (-5)) -- 5
  {- 上边 abs 替换 x、-5 替换 y 过程称为 β 化简 -}
  print (f'1 1 2)
  print (f'2 1 2)
  {- α 替换（α-conversion）所指的是在不出现命名冲突的前提下可以将函数的参数重新命名。
   β 化简（β-reduction 也译作 β 归约）则指的是参数到函数体的替换。
   应用参数 N 于函数 λx → M，相当于在不出现命名冲突的前提下，把 M 中出现的 x 替换为 N。
   η 化简（η-reduction 也译为 η 归约）可以用来消去那些冗余的 λ 表达。
   在定义函数时，可以将一个参数传给函数 M，进行计算的函数和 M 本身是同一个函数 -}

-- 2.4.1 λ 表达式的应用
test2_4_1 :: IO()
test2_4_1 = do
  putStrLn "-----[ test2_4_1 ]-----"
  {- λ 表达式的应用主要有两个：
    1. 构造一个没有名字的函数匿名函数（anonymous function）
    2. 对于柯里化函数，在不给定前一个参数的前提下给定后一个；-}
  print (map (\x -> 2 * x + 7) [1..10])

{- 其中 a、b、c 是三角形的三边长度。由于 p 的值仅定义一次就好，这种情况就可以用 let..in.. 在函数定义中做替换。
海伦公式在 Haskell 中可以定义为: -}
s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2 in sqrt (p * (p - a) * (p - b) * (p - c))
{- 除了用 let..in.. 以外，还可以使用 where。依旧以海伦公式为例，可以先定义好函数，然后在 where 关键字后边定义参数 p 作为函数定义的补充。
就像数学公式里的括号一样，where 是对公式的一个补充说明 -}
s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p - a) * (p - b) * (p - c)) where p = (a + b + c) / 2

-- 2.4.2 参数的绑定
test2_4_2 :: IO()
test2_4_2 = do
  putStrLn "-----[ test2_4_2 ]-----"
  {- 用户不仅可以绑定表达式，还可以绑定函数 -}
  print (let f x = x + 1 in f 5) -- 6
  {- 多个表达式绑定可以用分号隔开 -}
  print (let x=2; y=2 in x+y) -- 4
  {- 使用 let..in.. 绑定要注意命名捕获（name capture）。表面上看这里计算的是三个 x 值相乘，但是这里的 x 值是不等的，
  第一个 x 的值是 6，后两个 x 的值是 2，所以计算的结果是 6 × 2 × 2，结果是 24 -}
  print (let x = 6 in x * let x = 2 in x * x) -- 24
  {- 但是，如果定义一个函数，用了 where 绑定，那么它在函数的全部的范围都是有效的。当然，也不可能用同一个变量的名。
  但是，where 也可以像 let 那样有多级，即在 where 内定义的函数内又有 where，读者可以自己尝试一下，但是这种用法并不常见。 -}

isTwo :: Int -> Bool
--isTwo n = if n == 2 then True else False
isTwo = (==2)
{- Haskell 里的 if..then..else 顺序式编程语言有些不同，就是 else 后的表达式不可省略。
也就是说，必须定义条件成立的时候返回的值，也必须定义条件不成立的时候返回的值，并且返回的类型必须相同，这样一定程度上保证了函数定义的完整性。
在顺序式语言中，if..then..else.. 是一种表达式结构，而在 Haskell 中可以理解为一个运算符也可以理解为一个函数
（运算符与函数其实是等价的，这一点将在后面的小节中详细说明），这个运算符既不像加号那样处于两个参数中间为中缀运算符（infix operator），
也不像对数函数那样位于一个参数前边为前缀运算符（prefix operator）。它是一个混合位置运算符（mixfix operator 或者译为混缀运算符），
它是 Bool -> a -> a -> a 类型的一个函数，即需要一个条件参数，若满足，则返回第二个参数为结果；否则返回第三个。 -}

-- 2.5.1 条件表达式
test2_5_1 :: IO()
test2_5_1 = do
  putStrLn "-----[ test2_5_1 ]-----"
  print (isTwo 2)
  print (isTwo 7)

{- 情形分析表达式是用 case 与 of 关键字来对一个类型中不同的值进行讨论的，它与顺序式编程里的 switch .. case 类似。
区别就是，Haskell 不用像 switch 那样选择到一个条件继续向下运行而不自动跳出。因此在 Haskell 中，不需要用到 break 关键字。
顺序式语言中 default 关键字和 Haskell 里通配符类似，即一个下划线“_”，表示除了上面的匹配外，匹配所有的值。-}
month :: Int -> Int
month n = case n of
  1 -> 31
  2 -> 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> error "invalid month"

-- 2.5.2 情形分析表达式
test2_5_2 :: IO()
test2_5_2 = do
  putStrLn "-----[ test2_5_2 ]-----"
  print(month 2)

{- 守卫表达式（guarded expression）是使用 | 将函数的参数按特定的条件分开，| 像一个守卫一样，如果不能满足条件，
它绝不会让不符合条件的表达式运算。不同条件的守卫表达式的 | 需要对齐。 -}
abs' :: (Num a, Ord a) => a -> a
abs' n
  | n > 0 = n
  | otherwise = -n
{- otherwise 是匹配时的默认定义。因为 | 后的一定是一个布尔类型，所以 otherwise 是布尔类型的值并且它的值很明显是 True。
有兴趣的读者可以打开 Prelude.hs，找到 otherwise 的定义。这里需要说明的是，在定义函数时可能有很多条件，
但是如果有多个条件同时满足 Haskell 只会匹配第一个。 -}

-- 2.5.3 守卫表达式
test2_5_3 :: IO()
test2_5_3 = do
  putStrLn "-----[ test2_5_3 ]-----"
  print(abs' (-2))

month' :: Int -> Int
month' 1 = 31
month' 2 = 28
month' 3 = 31
month' 4 = 30
month' 5 = 31
month' 6 = 30
month' 7 = 31
month' 2 = 40 -- 不会被匹配的定义
month' 8 = 31
month' 9 = 30
month' 5 = 20 -- 不会被匹配的定义
month' 10 = 31
month' 11 = 30
month' 12 = 31
month' _ = error "invalid month"

-- 2.5.4 模式匹配
test2_5_4 :: IO()
test2_5_4 = do
  putStrLn "-----[ test2_5_4 ]-----"
  print(month' 2)
  print(month' 5)

{- 2.5.5 运算符与函数
运算符不过是一些规定了可以放在参数中间或者末尾的函数，并且用了一些特殊的符号表示，将它们写在最前端实质是一样的。
例如在 GHCi 里输入 (+) 5 6 与 5 + 6 是一样的。读者可以将一些二元函数用反单引号（`）（位于数字 1 键左侧的 键）来转换成位于参数中间的运算符。
比如，5 `div` 2 表示 div 5 2，其中 div 是整数除法函数。同理，取余函数 mod 也可以写为中缀运算符的形式，5`mod`2 与 mod 5 2 是一样的。
运算符号其实不过是为了简化书写而约定的，本质还是函数，在 Haskell 里函数与运算符是等价的。-}

{- 当然，这不是 Haskell 中所有的运算符，运算符只是定义了优先级的普通函数，在后面将见到更多的运算符。
因为 Haskell 中的运算符是可以自己定义的，下面我们就来定义自己的运算符。虽然 Haskell 中通过类型类支持函数的重载，
但 Haskell 不像 C++ 那样非常自由地支持函数与库运算符重载的。比如，不可以随意地重载加号，因为它的类型必须是 Num a=> a -> a -> a。
但在 Haskell 中，可以自由地定义自己的运算符号，但要声明它的结合性是怎样的，优先级是多少。
声明运算符的关键字有三个：infixl 为左结合，infixr 是右结合，infix 为无结合。定义时先用这三个关键字说明结合性，再声明优先级，
最后给出运算符的符号。如果需要定义多个结合性与优先级相同的运算符，那么它们之间要用逗号分开 -}
infixr 5 <->, <+>

(<->), (<+>) :: Int -> Int -> Int
(<->) x y = x - y
(<+>) x y = x + y

infixr 4 `foo` -- Haskell 支持声明中缀函数的结合性与优先级

foo a b = a + b

-- 2.5.6 运算符与自定义运算符
test2_5_6 :: IO()
test2_5_6 = do
  putStrLn "-----[ test2_5_6 ]-----"
  {- (!!) 从一个列表中取出第 n 个元素（从 0 开始）。如果输入的数大于等于列表的长度就会出现错误。 -}
  print([1,2,3,4,5]!!3) -- 4
  {- (.) 是复合函数的运算符号，将在第7章高阶函数中讨论 -}
  {- (^) 在 Haskell 里表示乘方。它的类型为 (Num a, Integral b) => a -> b -> a。
  由它的类型可知，底数可以是一个任意的数类型，而指数一定要是整数类型，并且必须为正整数。 -}
  print(100 ^ 2)
  {-  (^^) 也为乘方函数，这里规定底数 a 必须是 Fractional 类型类的实例。它的类型为 (Integral b, Fractional a) => a -> b -> a。-}
  print(0.5 ^^ 2)
  {- (**) 还是乘方函数，但它的类型是 Floating a => a -> a -> a，意思就是说，这个函数的两个参数都可以为小数。-}
  print(0.64 ** 0.5) -- 0.8

  {- mod 和 rem 是取余函数，div 和 quot 是求商函数，但是在计算负数的时候，运算结果会有些不同。 -}
  print(div (-12) 5) -- -3
  print(mod (-12) 5) -- 3
  print(quot (-12) 5) -- -2
  print(rem (-12) 5) -- -2
  {- 可以看出，div 与 mod 是一组，因为商-3 乘以 5 再加上-3 正好是 12。同理，quot 与 rem 是一组。
  在计算的过程中，在保证余数 r 的绝对值小于除数的情况下，div 总是要结果逼近于负无穷，而 quot 总是需要将结果逼近 0。-}
  {- 很多情况下，一定要对负数加括号，否则出现会有一些“意外”的结果，比如-2 `mod` 6 我们会得到-2，而我们想要的结果是 4，
  注意，减号-的优先级比 mod 要低，所以这里实际上在计算-(2 `mod` 6)。因为 Haskell 的语法中使用了大量的中缀运算符，
  所以在今后定义函数时要注意运算符的优先级，否则会因为括号遗漏或优先级没考虑而导致程序出现问题，从而没有得到预计的结果。-}
  print(-2 `mod` 6) -- -2
  {- (:) 正如前面所提到的，元素列表连接运算符也是一个函数，并且是一个有着类型 a -> [a] -> [a] 的函数，即将一个新的元素加入一个列表的第一个位置。-}
  print(1 : [1,2,2]) -- [1,1,2,2]
  {- (++) 列表连接符，顾名思义是连接两个列表的，类型为 [a] -> [a] -> [a]。 -}
  print([1,2] ++ [1,2]) -- [1,2,1,2]
  {- /= 表示的是不等于，不要与其他语言混淆，写成!= 或者 <> 都是错误的写法。-}
  print(1 /= 2)
  {- ($) 这个函数是很实用的。当有多个函数应用时，Haskell 默认计算为左结合，而这里因为 ($) 有着最低的优先级，并且是右结合，
  那么使用 ($) 就可以避免使用过多的括号来定义函数。比如：当需要定义 f (g (h x)) 时，可以写成 f $ g $ h x，
  使得代码更清楚并且十分方便，相当于把 x 输入给 h，所得结果输入给 g，依次类推。-}
  let f = (*2)
  let g = (+5)
  print(f (g 5)) -- 20
  print(f $ g 5) -- 20
  print(10 <-> 5 <-> 2) -- 7

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
  test2_3_1
  test2_3_2
  test2_3_3
  test2_4
  test2_4_1
  test2_4_2
  test2_5_1
  test2_5_2
  test2_5_3
  test2_5_4
  test2_5_6
