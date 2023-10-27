## 类型依赖
Haskell可以实现类型依赖，例如当需要一种特定长度的数组时，Haskell可以实现一个这样的Vec:
```haskell
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
- data Nat = Zero | Succ Nat
- data Vec :: Nat -> * -> * where
  VNil  :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a
```
Vec 3 a和Vec 4 a就是不同的类型了，编译期就可以判断。在不支持依赖类型的语言(例如java)则只能在运行时来人工判断。
## 代数类型(ADT)
Haskell支持代数类型，当想要实现一个三选一的类型时，Haskell中可以这样:
```haskell
data SomeType = Type1 | Type2 | Type3
```
在不支持代码类型的语言中，只能进行强制转换，无法在编译期进行类型判断。

## 类型类(Typeclass)
### 隐式绑定
在Haskell中，你可以为任何类型实现指定的Typeclass，而不需要改变原来的类型定义的代码。但是在java中你只能在类型定义的时候就指定要实现某个interface。

### 类型参数
在Haskell中，你可以为Typeclass指定参数，例如你可以定义一个叫Container的Typeclass:
```haskell
class Container c a where
  isEmpty :: c a -> Bool
```
在这个java中是不能实现的，在java中只有泛型，而没有类型参数。

### 函数派发
为不同的类实现相同的Typeclass，可以在运行时自动选择合适函数实现。例如Show这个Typeclass中定义了show函数，show函数可以根据传入的参数自动选择函数实现。在java中虽然可以使用interface实现类似的功能，但是由于不支持隐式绑定，在使用时受到限制。