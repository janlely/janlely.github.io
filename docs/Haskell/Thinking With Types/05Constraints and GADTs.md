## Constraints
Constraints是一种KIND，但是它不同于TYPE也不同于data kind，是一个看上去很奇怪的东西。它有两种形式，一种是出现在(=>)的左边，就像Show a, Eq a等等：
```haskell
(==) :: Eq a => a -> a -> Bool
sequenceA :: (Applicative f, Traversable t) => t (f a -> f (t a)
```
还有一种是Type equalities,(~)：
```haskell
five :: Int 
five = 5

five_ :: (a ∼ Int) => a
five_ = 5
```
使用(a ~ Int)需要开启-XGADTs。five的类型是Int，five_的类型是a并附带一个约束条件a ~ Int。可能不会有人写five_这样的函数，但"Type equalities"也是一种特性，而且它还满足交换律和传递性。

## GADTs
GADTs是haskell的类型系统中的一种扩展，它允许显示的写出类型构造函数的类型签名，下面是一个最简单的表达式求值的例子：
```haskell
data Expr a where
  LitInt ::Int -> Expr Int 
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
```
再定义一个求值函数:
```haskell
eval :: Expr a -> a
eval (LitInt i) = i
eval (LitBool b) = b
eval (Add x y) = eval x + eval y
eval (Not x) = not $ eval x
eval (If b x y) = if eval b
                    then eval x
                    else eval y
```
这样就有了一个非常简单的代码解释器:
```haskell
ghci> eval $ If (LitBool False) (LitInt 1) (Add (LitInt 2) (LitInt 3))
5
ghci> eval $ If (LitBool True) (LitInt 1) (Add (LitInt 2) (LitInt 3))
1
```
事实上GADTs是Type equalities的语法糖，上面的Expr等价于：
```haskell
data Expr a
  = (a ~ Int) => LitInt a
  | (a ~ Bool) => LitBool a
  | (a ~ Int) => Add (Expr a) (Expr a)
  | (a ~ Bool) => Not (Expr a)
  | If (Expr Bool) (Expr a) (Expr a)
```

## Heterogeneous Lists
Heterogeneous Lists(异构列表)，是指一个元素可以是不同类型的列表。借助于GADTs可以实现异构列表:
```haskell
data HList (t :: [Type]) where
  HNil :: HList '[]
  (:#) :: a -> HList t -> HList (a ': t)
infixr 5 :#
```
HList被定义成一个参数为[Type]的列表，[Type]可以使用Type-level的List来实现，如(':)函数是一个类型级别构造器:
```haskell
ghci> :k! True ': '[]
True ': '[] :: [Bool]
= '[ 'True]
```
* (':)并不是一个type family，所以不能像AppendSymbol这样查看它的定义：
    ```haskell
    ghci> :k AppendSymbol
    AppendSymbol :: Symbol -> Symbol -> Symbol
    ```
HList可以存放不同类型的元素:
```haskell
ghci> :t 1 :# Just 2 :# HNil
1 :# Just 2 :# HNil :: (Num a1, Num a2) => HList '[a1, Maybe a2]
```
如何为HList实现Eq, Ord和Show呢？
* Eq
    ```haskell
    instance Eq (HList '[]) where
      HNil == HNil = True

    instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
      (x :# xs) == (y :# ys) = x == y && xs == ys
    ```

* Ord
    ```haskell
    instance Ord (HList '[]) where
      compare HNil HNil = EQ

    instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
      compare (x :# xs) (y :# ys) = compare x y <> compare xs ys
    ```

* Show
    ```haskell
    instance Show (HList '[]) where
      show HNil = "[]" 

    instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
      show (x :# xs) = let rt = show xs
                         in if rt == "[]"
                              then "[" <> show x <> "]"
                              else "[" <> show x <> "," <> drop 1 rt 

    ```
值得注意的是，HList与List有一个重要区别：在List里面[]、[1,2,3]、[4,5]都是[Int]类型，与长度无关；但是在HList中，[]、[True,Just 1]、[1,Maybe 2]的类型都不一样，HList的类型中包含了所有元素的类型，所以在实现Eq和Ord的时候需要写两条instance语句。由于[]和[True]属于不同的类型，所以不能执行(==)函数，下面的代码将无法编译:
```
ghci> HNil == (True :# HNil)
<interactive>:78:10: error:
    • Couldn't match type: '[Bool]
                     with: '[]
      Expected: HList '[]
        Actual: HList '[Bool]
    • In the second argument of ‘(==)’, namely ‘(True :# HNil)’
      In the expression: HNil == (True :# HNil)
      In an equation for ‘it’: it = HNil == (True :# HNil)
```

对于Eq和Ord这类的class，需要写多条instance语句并不是很舒服，原因Eq和Ord这种Constraint只能约束一个类型，我们可以使用type family来实现一个对于[Type]的Constraint:

```haskell
type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)
```

然后可以使用一条instance来分别实现Eq和Ord:
```haskell
instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (x :# xs) == (y :# ys) = x == y && xs == ys

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (x :# xs) (y :# ys) = compare x y <> compare xs ys
```