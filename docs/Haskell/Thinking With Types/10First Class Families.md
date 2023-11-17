## Defunctionalization(去函数化)
Defunctionalization 是一种程序转换技术，它可以将高阶函数转换为一阶函数。这种技术最初是为了将程序转换为可以在更简单的机器上执行的形式而开发的。

在 Haskell 的类型编程中，defunctionalization 通常指的是将函数类型转换为数据类型的过程。这样做的目的通常是为了解决 Haskell 中不能直接对函数类型进行模式匹配的问题。

例如，假设我们有一个高阶函数 apply，它的类型是 `(a -> b) -> a -> b`。我们可以为这个函数创建一个对应的数据类型：
```haskell
data Apply a b = Apply (a -> b) a
```
我们再创建一个class:
```haskell
class Eval l t | l -> t where
  eval :: l -> t
```
给Apply创建Eval的instance:
```haskell
instance Eval (Apply a b) b where
  eval (Apply f x) = f x
```
效果:
```bash
ghci> eval (Apply (*2) 2)
4
```
同样，我样可以为`fst`函数创建Fst数据类型并实现Eval:
```haskell
data Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
  eval (Fst (a, _))  = a
```
```bash
ghci> eval (Fst (1,2))
1
```
高阶函数也可以去函数化，例如作用在List上的map函数:
```haskell
map :: (a -> b) -> [a] -> [b]
```
我们可以定义一个类型:
```haskell
data MapList dfb a = MapList (a -> dfb) [a]
```
这里我之所以使用`dfb`而不使用`b`是为了强调它是一个去函数化的类型(defunctionalized b)，然后给这个`MapList`实现`Eval`:
```haskell
instance Eval dfb b => Eval (MapList dfb a) [b] where
  eval (MapList f []) = []
  eval (MapList f (a:as)) = 
    eval (f a) : eval (MapList f as)
```
之拟要添加`Eval dfb b`是因为`dfb`是一个去函数化的类型，所以`f a`的结果需要使用`eval`函数来求值。

看一下效果:
```bash
ghci> eval (MapList Fst [("hello", 1), ("world", 2)])
["hello","world"]
```

## Type-Level Defunctionalization
