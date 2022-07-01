# HaskellProject

这个项目主要就是自己练习一下 Haskell 啦，从 Java -> Scala -> Haskell 一路过来的，想借此了解一下函数式编程到底怎么写比较好。

项目本身没啥可说的，我看网上 Haskell 相关的文章好少，下面就简单写一下自己环境配置相关的趟坑内容吧~ 供大家参考

使用 IntelliJ IDEA 配置 Haskell 环境主要分下面几步：

1. 安装 Stack
2. 在 IDEA 上安装 intellij-haskell 插件

## Stack 相关

### 安装

看 [官方文档](https://docs.haskellstack.org/en/stable/README/#how-to-install) 安装即可，Windows 可以直接下载一个安装器（文档里面 Windows 64-bit Installer 的链接）

安装器就是一路下一步就是了，不过其中有一个选项 `Set %STACK_ROOT% to C:/sr` 会使一些文件的路径安装在 `C:/sr` 下。下面会讲到。

### 使用 Stack 初始化一个 Haskell 项目

命令如下，my-project 可以替换成你想要创建的项目名称。后两步是编译和运行

```sh
stack new my-project
cd my-project
stack setup

stack build
stack exec my-project-exe
```

如果你想用 REPL 的话，使用下面命令安装一个 ghci 即可：

```sh
stack ghci
```

### 配置清华大学镜像

Stack 默认的下载路径很慢，或者直接就是下不了（你懂的），所以上面的初始化项目你很可能会失败（比如 stack setup 过程中需要下载 GHC 编译器 —— 顺便提一句，其实正因为如此，你也不需要想某些教程里面写的那样自己去下个 GHC），这时，可以配置成清华大学的镜像：

(以下引用 [清华大学开源软件镜像站](https://mirror.tuna.tsinghua.edu.cn/help/stackage/) 的描述)

> 修改 `~/.stack/config.yaml`（在 Windows 下是 `%APPDATA%\stack\config.yaml`）, 加上:
> 
> ```
> setup-info-locations: ["http://mirrors.tuna.tsinghua.edu.cn/stackage/stack-setup.yaml"]
> urls:
> latest-snapshot: http://mirrors.tuna.tsinghua.edu.cn/stackage/snapshots.json
> 
> snapshot-location-base: https://mirrors.tuna.tsinghua.edu.cn/stackage/stackage-snapshots/
> ```
> 此外，还需要手动下载 https://mirrors.tuna.tsinghua.edu.cn/github-raw/fpco/stackage-content/master/stack/global-hints.yaml 到 `~/.stack/pantry/global-hints-cache.yaml`（在 Windows 下是 `%APPDATA%\stack\pantry\global-hints-cache.yaml`）。注意文件名不同。这是由于 stack 暂时不支持配置该文件的上游地址。该文件需要在每当第一次用到新版本的 GHC 时更新。

需要注意的是，这里所写的 Windows 路径，如果你在 Stack 安装过程中选择了那个修改 %STACK_ROOT% 路径的选项的话，`%APPDATA%` 要相应改成 `C:/sr`

## intellij-haskell 相关

其实很多功能在 [插件的 README 文档](https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md) 里面都写了，要是可以看得懂的话，就直接啃英文就好了。只是一些细节需要注意，不想看英语或者遇到一些问题的话，可以参考我的配置。

### 相关的包

intellij-haskell 中很多功能（例如 Ctrl + Shift + L 的代码格式化功能由 ormolu 提供，代码检查功能由 HLint 提供等）需要一些包（hlint、hoogle、ormolu、stylish-haskell 这些。以前一些文章里面提到 hindent，现在应该不需要了？），具体可以在 Settings -> Languages & Frameworks -> Haskell 中看到，可以覆写它们的安装路径（这个可以在安装命令执行最后看到，我的对应是 %APPDATA%\Roaming\local\bin 下面）。

安装这些包的语句：

```sh
stack install hlint
stack install hoogle
stack install ormolu
stack install stylish-haskell
```

> 不过我现在很多功能也还没整明白，比如代码格式化，我现在中文会直接变乱码。然后 READ ME 文档里面说的很多 Navigate 的功能好像在同一个文件内也没有生效（跨文件好像确实可以找得到）。
> 
> 安装过程中我是报错了就重新跑一遍命令，看 Progress 的总数却是接着上次的剩余量。不知道这样会不会有什么问题。
> 
> 之后慢慢研究一下，有结果在此处更新

#### hlint

Stack 安装 Hlint 等的过程中可能会报错：`<stderr>: commitAndReleaseBuffer: invalid argument (invalid character)`

如果你用的是 windows，然后 locale 是中文，使用 Stack build 编译 Haskell 可能会出现这个错误。

解决方法很简单：命令行输入

```sh
chcp 65001
```

即可。

#### stylish-haskell

安装 stylish-haskell 的过程则遇到了 Stack 2.7.5 的默认 GHC 版本（9.0.2）低于 stylish-haskell 的要求（>= 9.2 && < 9.3，可以在 [Hackage 网站](https://hackage.haskell.org/package/stylish-haskell) 上查到）的问题。

查看 GHC 的版本，可以去访问它们的 [官网](https://www.haskell.org/ghc/)

```sh
stack --version # Stack 版本
stack upgrade # 更新 Stack（Stack 目前最新版本 2.7.5，和 GHC 9.2.3 组合未经测试）
stack ghc -- --version # 查询 GHC 版本
stack --resolver ghc-9.2.3 setup # 指定安装 9.2.3 版本的 GHC
# stack config set resolver ghc-9.2.3 # 指定 resolver 将当前使用的 ghc 切换成 9.2.3 这个版本，缺的依赖更多了……
# stack config set resolver lts # 还原 resolver 为 lts（其实是 lts-19.13）
stack --compiler ghc-9.2.3 install stylish-haskell
```

到这一步的话，安装不成的，会报 ghc-lib-parser-ex 这个依赖的版本还是对不上（解决了还有个 ghc-lib-parser 的，具体解决方法如下）。

处理方法其实就是按报错中所说的，去 sr 文件夹下的 global-project 文件夹的 stack.yaml 中添加如下语句：

```yaml
extra-deps:
- ghc-lib-parser-9.2.3.20220527@sha256:f43ed294dd3fcd133037bc86e8976ada653c8ab0f248dfbddb3bf45c9ae14bff,12720
- ghc-lib-parser-ex-9.2.1.0@sha256:cf9082b394e67f3396d0dd7c5d14e3aa43d95efbb74e75d51905f32f694f6523,3493
```

> 使用 `stack --compiler ghc-9.2.3 install stylish-haskell` 安装 stylish-haskell 的过程中一直在报：
> 
> ```
> --  While building package base-compat-0.11.2 (scroll up to its section to see the error) using:
>      C:\sr\setup-exe-cache\x86_64-windows\Cabal-simple_Z6RU0evB_3.6.3.0_ghc-9.2.3.exe --builddir=.stack-work\dist\8a54c84f build --ghc-options " -fdiagnostics-color=always"
>    Process exited with code: ExitFailure 1
> ```
> 
> 看输出，好像是 Haskell GHC 9.2 以后废弃 Option 推荐使用 Maybe 导致的 base-compat-0.11.2 一直 build 不成功？
> 
> ```
> base-compat > src\Data\Semigroup\Compat.hs:25:5: error:
> base-compat >     Not in scope: type constructor or class ‘Option’
> base-compat >    |
> base-compat > 25 |   , Option(..)
> base-compat >    |     ^^^^^^^^^^
> base-compat >
> base-compat > src\Data\Semigroup\Compat.hs:26:5: error: Not in scope: ‘option’
> base-compat >    |
> base-compat > 26 |   , option
> base-compat >    |     ^^^^^^
> ```
>
> 用 `stack --compiler ghc-9.2.3 install stylish-haskell` 的话，Progress 到最后一点进度（估计对应就剩 base-compat 的），还是编译失败。
> 
> 我就想这个 GHC 版本用 9.0.2 的行不行，就试了一下。到头来发现，直接 `stack install stylish-haskell` 居然就可以了…… 不知道是不是只在 stack.yaml 加 extra-deps 就可以，不需要 ghc-9.2.3 ？有空的话研究下
> 
> 目前按如上经历下来，安装好了 stylish-haskell


### 编译运行

一开始不知道插件支持，就直接在终端手动调用下面命令：

```sh
stack build # 编译
stack exec HaskellProject-exe # 运行
```

其实直接在右上角 edit/add configuration 下拉框添加一个 Haskell Stack - Haskell Stack Runner。然后就可以直接点绿色三角按钮自动编译运行了