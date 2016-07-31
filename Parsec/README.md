# Parsec

之前在Parsec库的支持下用了很少的代码就写了个Markdown解析器，然后又想，Parsec这个库是怎么实现的呢？于是今天就手动用Haskell实现了一个简易版本的Parsec，核心功能都差不多，有较小的差别，比如解析的结果从原先的`Either ParseError a`改成了`Maybe a`，等。

并且把原来的Markdown程序移植到了新的Parsec下，只修改了很少的代码，基本和原先保持不变。

新的Parsec也只有110行代码。还是不得不感慨Haskell的表达能力之强。

