Monad和Arrow是两种不同的对于计算(*Computation*)的抽象，使用Monad和Arrow都可以实现组合和状态管理，本篇文章是对[Arrow Tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)的讲解，使用Arrow实现了猜字游戏(Hangman)。

Arrow强调计算的输入和输出之间的关系,而不关注计算的具体实现细节。函数构造子(->)是Arrow的实例，`a -> b`表示一个输入是`a`输出是`b`的Arrow，但是它不能管理状态，我们将创建一个可以管理状态的Arrow:
```haskell
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }
```

`Circuit`与`(->)`不同的是，它的输出除了一个值之外，还包含了自身的一个实现。我们先给它添加Categroy和Arrow的实现，后面我们将看到它的魔力。
```haskell
import Control.Arrow (Arrow((&&&), arr, first))
import qualified Control.Category as Cat

instance Cat.Category Circuit where
    id = Circuit (Cat.id,)
    (.) = dot
        where (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                let (cir1', b) = cir1 a
                    (cir2', c) = cir2 b
                 in (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(a, c) -> 
                            let (cir', b) = cir a
                             in (first cir', (b, c))

instance ArrowChoice Circuit where
    left orig@(Circuit cir) = Circuit $ \case 
        Left b -> let (cir', c) = cir b
                    in (left cir', Left c)
        Right d -> (left orig, Right d)
```

接下来我们实现一个`runCircuit`用来运行`Circuit`:
```haskell
-- version 1
-- runCircuit :: Circuit a b -> [a] -> [b]
-- runCircuit _ [] = []
-- runCircuit cir (x:xs) = let (cir', b) = unCircuit cir x
--                          in b : runCircuit cir' xs
-- version 2
runCircuit :: Circuit a b -> [a] -> [b]
runCircuit cir = snd . mapAccumL unCircuit cir
```
`runCircuit`接收一个`Circuit`和一个输入序列，返回一个输出序列。version 1是一个更加的直观的实现，而version 2则是利用`mapAccumL`的更简洁的版本。

问：`runCircuit`函数为什么接收输入序列，而不是单一的输入？

答：处理单一的输入只需要`(->)`即可，只有处理连续的输入才会涉及到状态的管理。

问：`runCircuit`和`fmap`有什么区别？
答：`runCircuit`在函数签名上和`fmap`很相似，但是`fmap`对于`[a]`中的每一个元素独立的应用一个函数，其输出`[b]`与`[a]`一一对应。而`runCircuit`接收一个序列`[a]`并返回一个序列`[b]`，不保证一一对应，更加具有一般性。

接下来让我们实现可以管理状态的`Circuit`。直观上我们很容易想到，我们可以使用一个初始状态`acc`和一个`a -> acc -> (b, acc)`函数来生成一个`Circuit`，我们将这个函数命名为`accum`:

```haskell
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \a -> let (b, acc') = f a
                               in (accum acc' f, b)
```
有一种特殊的`accum`函数，即`acc`的类型和`b`相同，将`(b, acc)`融合成`b`:

```haskell
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' b f = accum b (\a acc -> let b' = f a acc in (b', b'))
```

使用`accum`我们可以实现很多有趣的`Circuit`，例如我们可以实现一个求和的`Circuit`：

```haskell
total :: Num a => Circuit a a
total = accum' 0 (+)
```