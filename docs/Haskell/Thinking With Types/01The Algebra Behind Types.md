## ADT与类型基数(cardinality)
__Sum Types__
$$|Either\ a\ b|=|a| + |b|$$
__Product Types__
$$|(a,\ b)| = |a| + |b|$$
__Function Types__
$$|a\ \to \ b| = |b|^{|a|}$$
举个例子：假设$|a|=2$、$|b|=3$，那么对于$f :: a \to b$类型来说，一共有如下$3^2$种可能:
$$f(a_1) = a_1, f(a_2)=b_1$$
$$f(a_1) = a_1, f(a_2)=b_2$$
$$f(a_1) = a_1, f(a_2)=b_3$$
$$f(a_1) = a_2, f(a_2)=b_1$$
$$f(a_1) = a_2, f(a_2)=b_2$$
$$f(a_1) = a_2, f(a_2)=b_3$$
$$f(a_1) = a_3, f(a_2)=b_1$$
$$f(a_1) = a_3, f(a_2)=b_2$$
$$f(a_1) = a_3, f(a_2)=b_3$$
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
直观上来看，”为棋盘每个位置定义一个变量“与“定义一个函数，参数是棋盘的行、列，返回值是棋盘对应位置的值”这两种类型定义方式本质上是相同的。
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
