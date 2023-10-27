## ADT与类型基数(cardinality)
__Sum Types__
$$|Either\ a\ b|=|a| + |b|$$
__Product Types__
$$|(a,\ b)| = |a| + |b|$$
__Function Types__
$$|a\ \to \ b| = |b|^{|a|}$$
举个例子：假设$|a|=2$、$|b|=3$，那么对于$f :: a \to b$类型来说，一共有如下$3^2$种可能:
$$f(a_1) = b_1, f(a_2)=b_1$$
$$f(a_1) = b_1, f(a_2)=b_2$$
$$f(a_1) = b_1, f(a_2)=b_3$$
$$f(a_1) = b_2, f(a_2)=b_1$$
$$f(a_1) = b_2, f(a_2)=b_2$$
$$f(a_1) = b_2, f(a_2)=b_3$$
$$f(a_1) = b_3, f(a_2)=b_1$$
$$f(a_1) = b_3, f(a_2)=b_2$$
$$f(a_1) = b_3, f(a_2)=b_3$$
对于每一个$a$都有$b$种选择，所以是
$$\begin{matrix} \underbrace{ |b|\times|b|\times\cdots \times|b| } \\ |a|times \end{matrix} = |b|^{|a|}$$
### 同构(isomorphism)
当两个类型具有相同的**_类型基数_**时，这两个类型就是同构的
举个例子：定义一个表示"井字棋"游戏的棋盘的类型
```haskell
data TicTacToe a = 
  { topLeft :: a
  , topCenter :: a
  , topRight :: a
  , midLeft :: a
  , midCenter :: a
  , midRight :: a
  , botLeft :: a
  , botCenter :: a
  , botRight :: a
  }
  
```
可以发现TicTacToe的类型基数是$|a|^9$，因此下面的类型与上面的类型是同构的:
```haskell
data Three = One | Two | Three
data TicTacToe' a = 
  { board :: Three -> Three -> a }
```
因为，TicTacToe'的类型基数是:
$$|Three \to a|^{|Three|} = |a|^{3 \times 3} = |a|^9$$
直观上来看，”为定义棋盘每个位置定义一个变量“与“定义一个函数，参数是棋盘的行、列，返回值是棋盘对应位置的值”这两种类型定义方式本质上是相同的。
显而易见的是，使用第二种类型定义方式更简洁，并且使用起来也方便，比如要定义一个函数用来构造一个空棋盘，两种定义方式的实现方式如下:
```haskell
-- 第一种方式
emptyBoard :: TicTacToe a
emptyBoard = TicTacToe 
                 Nothing, Nothing, Nothing
               , Nothing, Nothing, Nothing
               , Nothing, Nothing, Nothing
-- 第二种方式
emptyBoard' :: TicTacToe' a
emptyBoard' = TicTacToe' $ const $ const Nothing
```

## Curry-Howard同构
### Exercise 1.4-i
```haskell
-- 给定一个f和一个g，可以实现一个(Either b c -> a)
func1 :: (b -> a) -> (c -> a) -> Either b c -> a
func1 f g (Left b) = f b
func1 f g (Right c) = g c

-- 反之亦然
func2 :: (Either b c -> a) -> (b -> a, c -> a)
func2 f = (leftFunc, righrFunc)
  where leftFunc b = f (Left b)
        rightFunc c = f (Right c)
```

### Exercise 1.4-ii
```haskell
-- 给定一个c -> (a,b)的函数，相当于给定了(c -> a)和(c -> b)两个函数
func1 :: (c -> (a,b)) -> (c -> a, c -> b)
func1 f = (fst . f, snd . f) 

-- 反之亦然
func2 :: (c -> a) -> (c -> b) -> (c -> (a,b))
func2 f g = \c -> (f c, g c)
```

### Exercise 1.4-iii
```haskell
func1 :: (b -> c -> a) -> (b,c) -> a
func1 f = \(b,c) -> f b c

func2 :: ((b,c) -> a) -> b -> c -> a
func2 f b c = f (b,c)
```

## Terms, Types, Kinds
every **Term** has a **Type**: 
- 4是一个Term,它的Type是Int

every **Type** has a **Kind**: 
- Maybe Bool是一个Type，Maybe的Kind是 TYPE -> TYPE
- Either的Kind是 TYPE -> TYPE -> TYPE
-  MaybeT的Kind是(TYPE -> TYPE) -> TYPE -> TYPE


## Variance

如果可以把a转换成b，那是否可以把T a转换成T b呢？T需要满足什么条件呢?
```haskell
--以下5种类型，只有第一种和第5种是可以的。
newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int) 
newtype T5 a = T5 ((a -> Int) -> Int)

```
**T1比较容易理解**: 
- T1 a表示一个函数 f1 :: (Int -> a)，通过T1 a再加一个f :: (a -> b)，可以得到T1 b。在T1 b中，我们先应用f1，然后再对结果应用f就可以了。

**T5相对更复杂一些**：
- T5 a表示一函数f5 :: ((a -> Int) -> Int)，通过T5 a再加一个f :: (a -> b)，可以得到T5 b。T5 a接收一个(a -> Int)的函数，它知道如何应用这个函数，最后返回一个Int，T5 b接收一个(b -> Int)的函数，它知道如何应用这个函数，但是它只知道给这个函数传入a类型的参数，所以通过f先把a转成b，再应用这个函数就可以了。

**不同的类型构造子T，存在三种不同的variance:** 
- Covariant:  任意(a -> b)的函数，可以提升为(T a -> T b)的函数  
- Contravariant：任意(a -> b)的函数，可以提升为(T b -> T a)的函数  
- Invariant:  

Covariant就是我们熟知的Functor，Functor的本质就是lift
```haskell
class Functor f where
  -- 这样写看起来更像lift
  --fmap :: (a -> b) -> (f a -> f b)
  fmap :: (a -> b) -> f a -> f b
```

对于类型T a是Covariant还是Contravariant，取决与a出现在正位置(positive position)还是负位置(negative position)。所有的类型都可以用**canonical representation**来表，对于三种基本的类型构造方式Either, (,)和(->)来说，它们的正负性分别是

|Type|a|b|
|--|--|--|
|Either a, b|+|+|
|(a,b)|+|+|
|(a->b)|-|+|

对于复杂类型则遵循**负负得正**的原则，例如(a,Bool) -> Int中的a就是负的。因为(->)左边是负，(a, Bool)中的a是正。