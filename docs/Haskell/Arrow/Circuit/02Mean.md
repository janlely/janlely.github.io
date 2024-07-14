这一节主要通过使用多种方法实现求平均值的`Circuit`来学习Arrow相关的语法，如`proc`,`-<`,`rec`等。

首先我们使用Arrow的相关函数来实现第一个版本:
```haskell
mean1 :: Num a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>^ (uncurry (/))
```
解释：使用`total`计算sum，使用`const 1 ^>> total`计算count，使用`(/)`计算均值


第二个版本，使用`proc`和`do`关键字:
```haskell
mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n
```
解释：相比于`mean1`，`mean2`更直观的表达了计算均值的过程

第三、四个版本，使用了`(|...|)`语法：
```haskell
mean3 :: Fractional a => Circuit a a
mean3 = proc value -> do
    (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
    returnA -< t / n

mean4 :: Fractional a => Circuit a a
mean4 = proc value -> do
    (t, n) <- (total -< value) &&& (total -< 1)
    returnA -< t / n
```
解释：`mean3`使用`(|...|)`语法，把任意多个Arrow组合成一个Arrow，`mean4`与`mean3`等介，是更加简化的版本

第五个版本，使用`rec`关键字创建递归绑定:
```haskell
mean5 :: Fractional a => Circuit a a 
mean5 = proc value -> do
    rec
        (lastTot, lastN) <- delay (0,0) -< (tot, n)
        let (tot, n) = (lastTot + value, lastN + 1)
        let mean = tot / n
    returnA -< mean
```
解释：对先求sum和count再计算平均值的方式不一样，`mean5`采用流式处理的方法，采用迭代更新的方法计算均值

第六个版本，显式地使用`loop`函数，取代之前使用`rec`的写法:
```haskell
mean6 :: Fractional a => Circuit a a 
mean6 = loop $ proc (value, ~(lt, ln)) -> do
    (lastTotal, lastCount) <- delay (0,0) -< (lt, ln)
    let (tot, n) = (lastTotal + value, lastCount + 1)
    let mean = tot / n
    returnA -< (mean, (tot, n))
```
解释：`mean5`中使用了`rec`，它最终会被翻译成使用`loop`函数的形式。这个版本显式地使用了`loop`函数，注意一定要使用`~(lt, ln)`来强制惰性求值，否则会无限递归。