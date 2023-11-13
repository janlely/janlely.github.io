## Coercions
在 Haskell 中，Coercible 类型类和 coerce 函数提供了一种方式，使得我们可以在 newtype 和其底层类型之间，或者在任何两种 Coercible 的类型之间进行无代价的转换。
```haskell
coerce :: Coercible a b => a -> b
```
使用newtype关键字创建的类型和它内部所包裹的类型在运行时是一模一样的，但是它们在编译期是不一样的，举个例子：
```haskell
newtype Reverse a = Reverse
  { getReverse :: a
  } deriving (Eq,Show)
```
`Reverse a`和`a`在运行时是一样的，不会产生额外的开销，但是我们可以分别为它们实现不同的Ord：
```haskell
instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a
```
我们可以实现与`a`相反的Ord，在用到Ord约束的场景时，`Reverse a`和`a`会表现的完全不同。newtype可以让我们利用类型系统来帮助我们更好的写代码，同时又不会损失性能。

## Roles
在containers库中有一个Map k v数据结构，它内部是使用平衡树来实现的，它有一个`insert`函数:
```haskell
insert :: Ord k => k -> v -> Map k v -> Map k v
```
整个Map k v在存储结构依赖于Ord k的实现，因此`Map k v`和`Map (Reverse k) v`的存储结构会完全不同。那么，到底`k`和`v`的区别在哪里呢？这时候“角色系统”就要出场了，正如类型系统确保了项（terms）被正确使用，以及种类系统（kind system）确保了类型逻辑正确一样，角色系统（role system）确保了强制转换（coercions）的安全性。

"角色系统"是 Haskell 中用于描述类型参数如何与类型转换（coercions）交互的系统。Haskell 中的类型参数可以有三个角色：nominal（名义的）、representational（表示的）和 phantom（幽灵的）。

- Nominal（名义的）：如果一个类型参数的角色是 nominal，那么我们不能在这个参数上进行任何类型转换。这意味着该类型参数必须严格匹配。例如，在 Data.Type.Equality 中定义的 (:~:) 类型，其类型参数的角色就是 nominal。

- Representational（表示的）：如果一个类型参数的角色是 representational，那么我们可以在这个参数上进行有限的类型转换。更具体地说，我们可以在 newtype 与其底层类型之间进行转换。例如，Data.Functor.Identity 中定义的 Identity 类型，其类型参数的角色就是 representational。

- Phantom（幽灵的）：如果一个类型参数的角色是 phantom，那么我们可以在任何两个类型之间进行转换，因为这个类型参数并未被实际使用。例如，Data.Proxy 中定义的 Proxy 类型，其类型参数的角色就是 phantom。

角色系统的设计是为了确保类型安全性。它会防止我们进行一些可能违反类型安全的类型转换。例如，如果我们尝试在两个 nominal 角色的类型之间进行转换，编译器会报错。

在之前的例子中
- `Reverse a`中的`a`的Role是Representational，意思就是`Coercible a b` => `Coercible (Reverse a) (Reverse b)`
- `Map k v`中的`k`的Role是nomial，即只有`k1 ~ k2`才有`Coercible (Map k1 v) (Map k2 v)`

另一个`role`是phantom，例如在`Proxy a`中的`a`是phantom。

不强制指定的情况下，编译器也会自动推导`role`，三种`role`中，phantom最弱，nominal最强，把一个弱的role升级到强的role被称为`strengthening`。编译器推断role的过程大致是这样：
- 假设所有的类型参数都是phantom
- (->)类型构造器的两个参数都是representational，data constructors可以被视为(->)
- (∼)的两个参数是nomial