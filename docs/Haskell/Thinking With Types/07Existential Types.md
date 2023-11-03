## Existential Types and Eliminators
Existential Types允许类型系统在特定的范围类忽略类型信息，利用这一特性我们可以实现Any类型
```haskell
data Any = forall a. Any a
```
Any可以表示任意类型，它提供了另一种实现异构列表的方式
```haskell
ghci> :t [Any 1, Any (Just 2), Any "hello"]
[Any 1, Any (Just 2), Any "hello"] :: [Any]
```
与HList不同的是，我们没有实现一种新的List类型，而是在现有的[a]上实现了异构列表。

通过GADTs，我们有另一种语法来实现Any
```haskell
data Any where
  Any :: a -> Any
```

Any忽略了其中存储的类型，利用Rank-N我们可以实现一个函数来消除这种隐藏，把内部的类型重新暴露出来
```haskell
elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a
```
elimAny函数中的`(forall a. a -> r)`表示`a`的类型由elimAny来决定，但是`r`的类型需要由caller来决定，因此我们并不能这样：
```bash
ghci> :t elimAny id

<interactive>:1:9: error:
    • Couldn't match type ‘a’ with ‘r’
      Expected: a -> r
        Actual: a -> a
      ‘a’ is a rigid type variable bound by
        a type expected by the context:
          forall a. a -> r
        at <interactive>:1:9-10
      ‘r’ is a rigid type variable bound by
        the inferred type of it :: Any -> r
        at <interactive>:1:1
    • In the first argument of ‘elimAny’, namely ‘id’
      In the expression: elimAny id
```
似乎elimAny并没有什么用，似乎并没有什么函数接收一个不确定的`a`并返回一个确定的`r`。但是，如果我们给`a`加一个限定，比如Show，那就不一样了：
```haskell
data Any = forall a. Show a => Any a

elimAny :: (forall a. Show a => a -> r) -> Any -> r
elimAny f (Any a) = f a
```
这时，我们可以这样：
```bash
ghci> :t elimAny show
elimAny show :: Any -> String
```
因为show函数接收任意的`Show a`生成`String`

## Dynamic Types
借助Existential Types和Eliminators，可以实现类似于Python风格的动态类型函数:
```haskell
data Dynamic where
  Dynamic :: Typeable a => a -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r)
       => Dynamic -> Dynamic -> (a -> b -> r) -> Maybe Dynamic
liftD2 a b f = fmap Dynamic . f <$> fromDynamic @a a <*> fromDynamic @b b

plus :: Dynamic -> Dynamic -> Dynamic
plus a b = fromMaybe (error "bad types for plus") $ asum
  [ liftD2 @Int @Int a b (+)
  , liftD2 @String @String a b (++)
  , liftD2 @String @Int a b $ \s i -> s ++ show i
  , liftD2 @Int @String a b $ \i s -> show i ++ s
  ]
```
测试一下:
```bash
ghci> fromDynamic @Int (plus (Dynamic (1 :: Int)) (Dynamic (2 :: Int)))
Just 3

ghci> fromDynamic @String (plus (Dynamic "hello ") (Dynamic "world"))
Just "hello world"

ghci> fromDynamic @String (plus (Dynamic "hello ") (Dynamic (1 :: Int)))
Just "hello 1"
```
完全可以使用Haskell实现一个纯函数式的动态类型语言，从类型系统层面来讲，动态类型语言是一种只有单一类型的静态类型语言。

## Generalized Constraint Kinded Existentials
前面我们定义的Any类型和Dynamic类型:
```haskell
data Any where
  Any ::  Show a => a -> Any

data Dynamic where
  Dynamic :: Typeable a => a -> Dynamic
```
可以看到，它们长得一样，区别在于一个使用了Show约束，一个使用了Typeable约束。借助于-XConstraintKinds扩展，可以进行一个泛化：
```haskell

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow = Has Show
type Dynamic = Has Typeable
```
如果我同时需要多个Constraint应该怎么做呢？我们熟悉在函数签名中可以同时写多个Constraint：
```haskell
isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty
```
看起来我们可以这样:
```haskell
type MonoidEq a = (Monoid a, Eq a)
```
然而，这样并不行:
```bash
ghci> :t Has [True] :: Has MonoidAndEq

<interactive>:1:15: error:
    • The type synonym ‘MonoidAndEq’ should have 1 argument, but has been given none
    • In an expression type signature: Has MonoidAndEq
      In the expression: Has [True] :: Has MonoidAndEq
```
MonoidAndEq是一个类型别名(type synonyms)，它必须完全饱和(fully saturated)。然而我们可以用另一种方法来实现：
```haskell
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
```
这样就不会报错了
```bash
ghci> :t Has [True] :: Has MonoidEq
Has [True] :: Has MonoidEq :: Has MonoidEq
```


## Scoping Information with Existentials
在 Haskell 编程中，存在类型（Existential types）可以用来防止信息泄露到预期范围以外。例如，我们可以确保分配的资源不会逃离预设定的区域。我们可以利用类型系统证明 HTTP 会话令牌被限制在其请求上下文中，或者文件句柄在关闭后不再存在。我们可以将它用来实现范围限定机制。  
Haskell 的 ST 单子（Monad）就是这种方法的最著名例子，下面我们自己实现一遍ST单子，来看看存在类型的范围限制作用：
```haskell
newtype ST s a = ST
  { unST :: a
  }
```
这里我们使用了幽灵参数`s`来表示一个存在类型，后面我们会说明如何用到它。我们先实现Functor, Applicative和Monad
```haskell
instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  return = pure
  ST a >>= f = seq a . f $ a
```
借助IORefs来实现变量，我们把它包装在一个新的类型`STRef`中并提供一个`newSTRef`方法来创建变量:
```haskell
newtype STRef s a = STRef
  { unSTRef :: IORef a
  }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef 
```
接下来我们实现一些有用的函数:
```haskell
readSTR :: STRef s a -> ST s a
readSTR = pure . unsafePerformIO . readIORef . unSTRef


writeSTR :: STRef s a -> a -> ST s ()
writeSTR ref a = pure . unsafePerformIO $ writeIORef (unSTRef ref) a

modifySTR :: STRef s a -> (a -> a) -> ST s ()
modifySTR ref f = pure . unsafePerformIO $ modifyIORef (unSTRef ref) f
```
最后我们实现`runST`函数
```haskell
runST :: (forall s. ST s a) -> a
runST st = unST st
```
注意，如果写成`runST = unST`则会有一个报错，由于 unST 的类型是 forall s a. ST s a -> a，所以编译器会尝试将 runST 的类型 forall a. (forall s. ST s a) -> a 直接匹配为 unST 的类型。然而，这两个类型并不是完全一致的——它们的量化器（quantifier，也就是 forall 关键字）的顺序和作用域不同——所以编译器不能成功地匹配它们。而在 runST st = unST st 的形式中，你首先将 runST 的输入 st 绑定到一个变量，然后在 unST 的调用中使用这个变量。这样，unST 的 s 类型参数就能根据 st 的类型被正确地推导出来，从而使得 runST 和 unST 的类型能匹配。这个问题反映了 Haskell 类型系统中的一个重要概念，也就是“全称量化”（universal quantification）和“存在量化”（existential quantification）的区别。在类型系统中，全称量化的类型变量（例如 forall a. a -> a 中的 a）可以被任何类型替换，而存在量化的类型变量（例如 exists s. ST s a 中的 s）表示存在某个特定的、但我们不知道具体是什么的类型。在 runST 的类型中，s 就是一个存在量化的类型变量，而在 unST 的类型中，s 是一个全称量化的类型变量。  
接下来我们写一个简单的例子:
```haskell
safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTR ref (++ " world")
  readSTR ref
```
在ghci中运行一下:
```bash
ghci> runST safeExample
"hello world"
```