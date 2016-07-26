# Markdown 解析

最近在学Parsec这个库，感觉很强大，很灵活。*Real WorldHaskell*给的例子是用它解析CSV和JSON，感觉都太简单了，网上也看到用了很短的代码解析LRC歌词的。自己就想着能不能解析Markdown。

花了两天时间，实现了Markdown的大多数功能，能解析标题（＃）、粗体、斜体、分割线、无序列表和表格，列表可以根据tab缩进判断层次，暂时不支持空格缩进。

另外，也没办法实现数学公式和代码的语法高亮，嘿嘿嘿。

完成了这些功能只用了不到150行代码，这在任何命令式编程语言中都是不可能完成的吧。

### Usage
```
> ghc Main -o bin/Markdown
> ./bin/Markdown < test.md > test.html
```

