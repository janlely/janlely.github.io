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

<div style="display: flex; justify-content: center;">

|Type|a|b|
|--|--|--|
|Either a, b|+|+|
|(a,b)|+|+|
|(a->b)|-|+|

</div>


对于复杂类型则遵循**负负得正**的原则，例如(a,Bool) -> Int中的a就是负的。因为(->)左边是负，(a, Bool)中的a是正。