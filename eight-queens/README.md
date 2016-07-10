# 八皇后
八皇后问题是一个非常经典的算法问题，大一的时候接触到这个问题，当时用`C#`写了好久，好多循环，出了好多bug，最后勉强找到了一个解。后来接触到Prolog，在网上学到用Prolog只用8行就解决了这个问题，而且能找到全部解。现在用Haskell来解决这个问题，依旧十分精简（总感觉我这个写法已经复杂化了，一定有更简单的算法）。

`queens`是所有解的列表。可以通过命令
```shell
> ghci eight-queens.hs
```
```haskell
> import Control.Monad
> forM_ queens print
```
来输出全部解。

