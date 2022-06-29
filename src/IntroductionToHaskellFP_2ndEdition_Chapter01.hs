module IntroductionToHaskellFP_2ndEdition_Chapter01 where

-- 单行注释
{-
多行注释
IDEA 装 intellij-haskell 插件后，只能是方便写代码，本身不带编译功能，还是需要在终端里面使用如下脚本来编译运行（需要管理员权限）
编译：stack build
运行：stack exec HaskellProject-exe

其他一些指令：
启动REPL：stack ghci
-}
{-
GHCi 中的命令：
:load 简写为 :l，用来导入当前路径或者指定路径下的文件，但在 Windows 下要注意使用转义的反斜杠。
:reload 简写为 :r，用来重新导入当前的源代码文件。
通常，在保存了源文件后，GHCi 不会自动重新导入修改后的文件，用户可以很方便地使用 :r 来重新导入。
:cd 改变当前 GHCi 的路径。这样做就不用每一次都输入绝对路径来导入文件了。
:edit 用默认的文本编辑器编辑当前导入的文件。
:module 导入一个库，简写为 :m。
使用 :m +<module1> <module2> 与 :m -<module1> <module2> 来增加与移除不同的模块，只在 GHCi 中输入:m 会移除所有载入的模块。
:! 可以在 GHCi 中执行一些操作系统的命令
:quit 退出 GHCi。
:? 可以让 GHCi 输出帮助信息。
:t 查看后面加的内容的类型。例如 :t True, :t 'a'
-}
-- 1.4 第一个 Haskell 程序——HelloWorld
test1_4 :: IO ()
test1_4 = do
  putStrLn "-----[ test1_4 ]-----"
  putStrLn "Hello, World!"
