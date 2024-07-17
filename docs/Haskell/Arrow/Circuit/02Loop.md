这一节我们来介绍`ArrowLoop`，一种使用Arrow来处理循环的机制。

首先来看`Circuit`的`ArrowLoop`的实现:
```haskell
instance ArrowLoop Circuit where
    loop (Circuit cir) = Circuit $ \b ->
        let (cir', (c,d)) = cir (b,d)
        in  (loop cir', c)
```
这里使用了`tying the knot`来创建循环结构，`cir (b,d)`中的`d`引用自它自己的输出中的`(c,d)`。

函数的输入引用自己的输出？这不就死循环了吗？

如果在执行`cir (b,d)`时不需要使用`d`值，那么再加上惰性求值机制，`d`就不会被求值，因此也就不会限入死循环。常见的就是使用延迟输入的机制，把`d`的求值延迟一次，让第一次`cir (b,d)`执行时使用一个初始值，从而让计算结果`(cir', (c,d))`中的`d`不依赖于自身，就可以让程序运行下去。

后面一节有一个求平均值的例子很好的说明这一种情况:

首先实现一个延迟机制的`Circuit`:
```haskell
delay :: a -> Circuit a a
delay last = Circuit $ \this -> (delay this, last)
```
`delay`函数接收一个初始值，返回一个`Circuit`， 这个`Circuit`会将所有的输入都作为下一步的输出。

然后我们看这个求平均值的`Circuit`:
```haskell
mean6 :: Fractional a => Circuit a a 
mean6 = loop $ proc (value, ~(lt, ln)) -> do
    (lastTotal, lastCount) <- delay (0,0) -< (lt, ln)
    let (tot, n) = (lastTotal + value, lastCount + 1)
    let mean = tot / n
    returnA -< (mean, (tot, n))
```

它使用`loop`函数把一个`Ciruit (a, (c,d)) (b, (c,d))`转换成`Circuit a b`，其中在`Ciruit (a, (c,d)) (b, (c,d))`的实现逻辑中`(lt, ln)`既是输入又是输出，但是当前步骤没有用到`(lt, ln)`的值，把它传递给了`delay (0,0)`作为了下一步的输出，这样就实现了每一步的输入实际上来自于上一步的输出，同时第一步的输入又一个初始值来指定。

后续有机会再补充更多的使用`loop`的例子。