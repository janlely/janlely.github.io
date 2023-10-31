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