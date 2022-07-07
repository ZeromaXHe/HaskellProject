module IntroductionToHaskellFP_2ndEdition_Chapter03 (test3) where

{- 3.1 关键字 module 与 import 简介
比如，一些库函数与用户定义的函数可能是有命名冲突的（或者不想使用），这时可以使用 hiding 关键字将它们的定义隐藏，例如：
import Prelude hiding (catch)
有时，需要导入多个模块，但是其中的两个模块下有两个函数名称一样，可是又需要同时使用它们。这时，可以使用 qualified 关键字来对不同的模块命名，例如：
import qualified Test as T
当然，也可以不用 import 关键字，但在使用函数时需要给出完整的函数所在路径 -}

{- 半加法器（halfAdder） -}
hA :: Bool -> Bool -> (Bool, Bool)
hA a b = (a /= b, a && b)
{- 全加法器（fullAdder） -}
fA :: Bool -> Bool -> Bool -> (Bool, Bool)
fA a b c = let (axb, aab) = hA a b in
   let (axbxc, axbac) = hA axb c in (axbxc, aab || axbac)

-- 3.2 简易布尔值的函数
test3_2 :: IO()
test3_2 = do
  putStrLn "-----[ test3_2 ]-----"
  print(hA True False)
  print(fA True False True)

{- 在逻辑门电路中，有两个非常重要的逻辑门——nand（与非门）与 nor（或非门），即 not and 与 not or。
这两个逻辑门称为“通用逻辑门”（universal logic gate），它们的特殊在之处在于仅仅用 nand 与 nor 中的一个就可以定义出其他所有的逻辑门。
读者可以找一些逻辑门的内容来阅读。在本节里，将带领读者来试着实现一下。因为需要用 nand 与 nor 实现其他的逻辑门，
所以定义 nand 与 nor 的时候不能用到其他的函数，并且在定义其他函数的时候也不能用到之前定义过的函数，
只能用 nand 与 nor，但是可以用定义过的函数来推导。 -}
nand, nor :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True
nor False False = True
nor _ _ = False

not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor b b

and1, and2 :: Bool -> Bool -> Bool
-- and1 b1 b2 = not1 $ nand b1 b2
and1 b1 b2 = nand (nand b1 b2) (nand b1 b2)
-- and2 b1 b2 = nor (not2 b1) (not2 b2)
and2 b1 b2 = nor (nor b1 b1) (nor b2 b2)

or1, or2 :: Bool -> Bool -> Bool
-- or1 b1 b2 = nand (not1 b1) (not1 b2)
or1 b1 b2 = nand (nand b1 b1) (nand b2 b2)
-- or2 b1 b2 = not2 $ nor b1 b2
or2 b1 b2 = nor (nor b1 b2) (nor b1 b2)

xor1 :: Bool -> Bool -> Bool
-- xor1 b1 b2 = or1 (and1 b1 (not1 b2)) (and1 (not b1) b2)
-- xor1 b1 b2 = nand (not1 (and1 b1 (not1 b2))) (not1 (and1 (not1 b1) b2))
-- xor1 b1 b2 = nand (nand b1 (nand b1 b2)) (nand b2 (nand b1 b2))
xor1 b1 b2 = nand (nand b1 nb1b2) (nand b2 nb1b2) where nb1b2 = (nand b1 b2)

-- 3.3 与非门和或非门
test3_3 :: IO()
test3_3 = do
  putStrLn "-----[ test3_3 ]-----"
  print (not1 False)
  print (not2 False)
  print (True `and1` True)
  print (True `and2` True)
  print (True `or1` False)
  print (True `or2` False)
  print (True `xor1` False)

-- 3 基于布尔值的函数
test3 :: IO()
test3 = do
  test3_2
  test3_3