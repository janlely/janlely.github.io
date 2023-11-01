Haskell使用了Hindley–Milner类型系统，HM 类型系统有以下的主要特性：

- **静态类型检查**：类型检查在编译时完成，而不是在运行时。这有助于在程序运行之前发现类型错误。

- **类型推导**：编译器能够自动推导出表达式的类型，所以在很多情况下，程序员不需要明确地写出类型。这使得代码更简洁，同时仍然保持了静态类型检查的优点。

- **多态性**：HM 类型系统支持参数多态性，也就是通常所说的泛型。比如，你可以定义一个函数来对任何类型的列表进行排序，而不是对特定类型的列表进行排序。

- **安全性**：如果一个 HM 系统的程序类型检查通过，那么它就不会在运行时产生类型错误。

Haskell 和 ML 是使用 Hindley-Milner 类型系统的知名语言。尽管 HM 类型系统非常强大，但它并不能表达所有种类的类型关系。比如，它无法直接表达子类型关系或者依赖类型。因此，一些函数式语言，比如 Scala 和 Rust，选择了使用更复杂的类型系统，以便支持这些特性。

## Type Scoping
默认情况下，Haskell中的类型没有Scope，例如下面的代码无法编译：
```haskell
broken :: (a -> b) -> a -> b
broken f a = apply
  where apply :: b
        apply = f a
```
因为没有Scope，编译器不会认为apply的类型中的**b**和broken中的**b**是同一个**b**。Haskell提供了扩展来解决这个痛点：
```haskell
:set -XScopedTypeVariables
broken :: forall a b. (a -> b) -> a -> b
broken f a = apply
  where apply :: b
        apply = f a
```
这里的**forall a b.**是不能省略的。

-XScopedTypeVariables不能实现函数的“特化”，例如我想要一个专门用在Maybe上的fmap函数，我们可能会这样写：
```haskell
maybeFmap :: (a -> b) -> Maybe a -> Maybe b
maybeFmap = fmap
```
这样确实是可以编译，并且maybeFmap也确实是一个特化的fmap，但是在maybeFmap的实现中fmap并不是一个特化的函数，它还是一个多态的函数。
## Type Applications
-XTypeApplications这个扩展可以为多态函数指定类型：
```haskell
ghci> :set -XTypeApplications

ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> :t fmap @Maybe
fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

ghci> :t fmap @Maybe @Int @String
fmap @Maybe @Int @String
  :: (Int -> String) -> Maybe Int -> Maybe String
```
从上面的例子可以看出，使用@符号可以按顺序指定一个多态函数的类型。例子中fmap的签名是
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```
按顺序的话，第一个类型参数是f，第二个是a，第三个是b。

也可以跳过某个类型，指定后面的类型，例如：
```haskell
ghci> :t fmap @_ @Int @String
fmap @_ @Int @String
  :: Functor w => (Int -> String) -> w Int -> w String
```
使用@_占位符跳过了第一个参数f。

## Ambiguous Types
使用typeRep函数可以实现一个显示类型名称的typeName函数:
```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
import Data.Typeable
import Data.Proxy

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

```
测试typeName函数:
```haskell
ghci> typeName @Int
"Int"
ghci> typeName @(Maybe [Int])
"Maybe [Int]"
ghci> typeName @String
"[Char]"
```

## Non-Injectivity
考虑下面的type famlily
```haskell
type family AlwaysUnit a
  where AlwaysUnit a = ()
```
AlwaysUnit是没有逆的，没有办法从(AlwaysUnit a)中得到a的类型。类似这样的类型的特点被称为Non-Injectivity。类似的hash算法也是没有逆的。