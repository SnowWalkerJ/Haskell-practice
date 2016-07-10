# 2048游戏
使用Haskell实现，参考了网上的[Implementing the game 2048 in less than 90 lines of Haskell](http://gregorulm.com/2048-in-90-lines-haskell/)，但是我除了用户交互界面以外都没有引用IO，保证了基础函数的纯性。缺点是每次随机使用的都是相同的随机数种子，好吧这个问题好像有点严重。

操作：W A S D分别是上左下右。

