从一个例子开始:
```haskell
applyToFive :: (a -> a) -> Int
applyToFive f = f 5
```
上面的代码无法编译，报错内容是:
```
Couldn't match expected type ‘Int’ with actual type ‘a’
```
因为applyToFive被定义成可以接收一个任意类型的(a -> a)的函数f，f可以是(Int -> Int)，也可以是(Bool -> Bool)，然而只有当f是(Int -> Int)时，`f 5`才有意义。我们可以发现，applyToFive中的多态参数`a`其实要在调用之前就要由调用者确定下来，虽然可以是任意的类型，但是它不允许你传一个多态函数，比如`id`。


有没有办法可以让applyToFive函数接收`id`这样的多态函数呢？Haskell提供了RankNTypes扩展来实现这样的需求。开启这个扩展之后，applyToFive函数的实现可以写成这样：
```haskell
{-# LANGUAGE RankNTypes #-}
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5
```

## Ranks
-XRankNTypes最合适的理解就是，它使得“多态性”成了一等公民，它允许我们在任何允许类型的地方引入多态性，而不仅仅是在顶级绑定上。虽然这个特性是好的，但是它会使用类型推导变得困难，因此在使用Rank-N特性时必须写清楚类型签名。


究竟如何理解Ranks呢，rank指的是“多态”的深度，不使用多态的函数是rank-0的，我们熟知的`const :: a -> b -> a, head :: [a] -> a`这样的函数是rank-1的，上面我们重新定义的`applyToFive :: (forall a. a -> a) -> Int`函数是rank-2的，依此类推。

## Rank-N的语法
forall这个关键词的优先级比(->)要高，例如`forall a. a -> a`和`forall a. (a -> a)`是不相同的。

### Exercise 6.3-i
题目：给`Int -> forall a. a -> a`加上括号 

答案：`Int -> (forall a. a) -> a`


## The Continuation Monad
Continuation（延续）是一种编程模式，它允许你保存程序在某一时刻的状态，以便在将来某个时刻恢复执行。在函数式编程语言，如 Haskell 中，continuation 通常被表示为函数。

Continuation 在 Haskell 中有许多用途，以下是一些主要的例子：

1. **非局部退出**：Continuation 可以用于提前退出一个深层嵌套的计算。你可以在需要退出的地方调用 continuation，然后立即跳转到你想要的地方。

2. **异常处理**：Continuation 可以用于处理异常情况。你可以将异常处理函数作为 continuation 传递给可能会抛出异常的函数。如果发生异常，你可以调用这个 continuation。

3. **异步编程**：在异步编程中，continuation 可以用来处理异步操作的结果。例如，你可以将一个操作的结果作为参数传递给 continuation，这个 continuation 会在异步操作完成后被调用。

4. **协程（Coroutines）**：Continuation 可以用于实现协程。协程是一种可以挂起和恢复执行的计算单元，它们之间可以互相协作完成任务。通过使用 continuation，你可以在挂起点保存协程的状态，并在需要的时候恢复执行。

5. **Continuation Passing Style（CPS）转换**：CPS 是一种编程风格，其中每个函数都接受一个额外的参数，即 continuation。这种风格可以使得流程控制更加明确，也方便进行某些类型的优化。

总的来说，continuation 是一种强大的工具，可以用于处理各种复杂的控制流需求。



Continuation的底层原理来自于`a`和`(forall r. (a -> r) -> r)`是同构的，即存在两个函数：
```haskell
cont :: a -> (forall r. (a -> r) -> r)
cont a = \cb -> cb a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id
```
`cont`函数把`a`转成`(forall r. (a -> r) -> r)`, `runCont`函数把`(forall r. (a -> r) -> r)`转成`a`


使用`(forall r. (a -> r) -> r)`代替`a`被称为CPS(continuation-passing style)，由于同构(isomorphism)具有传递性，所以Identity a $\cong$ a $\cong$ cont a，既然Identity a是Monad，所以cont a也是Monad。使用newtype重新定义一个Cont a类型:
```haskell
newtype Cont a = Cont
  {unCont :: forall r. (a -> r) -> r}
```
分别为Cont a实现Functor, Applicative和Monad:
```haskell
instance Functor Cont where
  fmap f (Cont c) = Cont $ \cb -> c (cb . f)

instance Applicative Cont where
  pure a = Cont $ \cb -> cb a
  Cont f <*> Cont a = Cont $ \cb -> f $ \fab -> a $ \aa -> cb $ fab aa

instance Monad Cont where
  return = pure
  Cont a >>= f = Cont $ \cb -> a $ \aa -> unCont (f aa) cb

```
使用CPS可以解决javascript中的回调地狱(Pyramids of Doom)问题。假设有一个流程有四个步骤，每个步骤都要用到前一个步骤的结果，在javascript中可能会写成这样:
```javascript
step1(function(value1){
    step2(value1, function(value2){
        step3(value2, function(value3){
            step4(value3, function(value4){
                //final step
            })
        })
    })
})
```
使用CPS可以写这样:
```haskell
step1 :: a -> Cont b
step2 :: b -> Cont c
step3 :: c -> Cont d
step4 :: d -> Cont e

runSteps :: a -> Cont e
runSteps a = do
  b <- step1 a
  c <- step2 b
  d <- step3 c
  step4 d
```
同样是函数式写法，haskell中使用CPS和Monad实现了类似于命令式编程的风格。
