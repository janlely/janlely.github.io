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
之所以要添加`Eval dfb b`是因为`dfb`是一个去函数化的类型，所以`f a`的结果需要使用`eval`函数来求值。

看一下效果:
```bash
ghci> eval (MapList Fst [("hello", 1), ("world", 2)])
["hello","world"]
```

## Type-Level Defunctionalization
我们都知道通过type family实现的类型级函数是不支持高阶函数、部分应用和模式匹配的，但是通过去函数化的思路可以帮助我们实现类似的特性。
```haskell
type Exp a :: a -> Type
type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _1 ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing) = a


data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)
```

## Working with First Class Families
First Class Families(FCFs)是一种设计模式，用于在类型级别编程时使用类型族（type families）。这种模式通过模拟函数的柯里化（currying）和部分应用，使我们能够以一种更函数式的方式使用类型族。有趣的是，使用FCFs我们可以实现Type Level的Monad:
```haskell
data Pure :: a -> Exp a
type instance Eval (Pure x) = x
data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
infixr 0 =<<
type instance Eval (k =<< e) = Eval (k (Eval e))

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<
```
[first-class-families](https://hackage.haskell.org/package/first-class-families)库提供了一种强大的工具，使得Haskell的类型系统更加灵活和强大，例如:
```haskell
import Fcf

type family FromMaybe (a :: k) (m :: Maybe k) :: k where
  FromMaybe _ ( 'Just a) = a
  FromMaybe a 'Nothing = a

data FromMaybe :: k -> Maybe k -> Exp k
type instance Eval (FromMaybe a m) = FromMaybe a m
```