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

接下来我们开始着手开发Hangman游戏了。Hangman游戏游戏的规则是：
* 开始时，程序会随机从词典表中选择一个单词由玩家来猜，程序每一步会输出玩家猜测一次之后的结果，初始结果是由多个下划线连成的与单词长度相等的占位字符串
* 当玩家每输入一个字母时，如果字母与单词中某个下划线对应的中一个字母匹配，则该位置的字母会被复原，否则玩家会消耗一次尝试的机会
* 玩家完整的猜对单词视为胜利
* 玩家的尝试次数消耗殆尽视为失败

从主函数`hangman`开始，不难想到`hangman`函数和签名:
```haskell
hangman :: StdGen -> Circuit String (Bool, [String])
```
其中：
* StdGen是随机数种子，用来从词典中随机选择一个单词
* String表示用记的输入，通常是一个字母，如果用户输入多个字母则只会取第一个
* Bool用来表示游戏是否已结束
* [String]包含了游戏过程中程序的输出，包含了当前的单词、剩余的尝试次数、输赢的结束语


`hangman`的实现如下:
```haskell
hangman :: StdGen -> Circuit String (Bool, [String])
hangman g = proc input -> do
    word <- getWord g -< ()
    let letter = listToMaybe input 
    (guessed, failureCount) <- updateGuess -< (word, letter)
    hung <- total -< failureCount
    end <- delayEcho True -< not (word == guessed || hung >= attempts)
    let result | word == guessed = [guessed, "You won!"]
               | hung >= attempts = [guessed, livesLeft hung, "You died!"]
               | otherwise = [guessed, livesLeft hung]
    returnA -< (end, result)
```
其中：
* `getWord g`的类型是`Circuit () String`，用于随机产生一个单词
* `updateGuess`的类型是`Circuit (String, Maybe Char) (String, Int)`，它的输入`(String, Maybe Char)`表示要猜的单词和当前输入的字母(用户也可以直接按下回车)，它的输出`(String, Int)`表示当前猜测的结果和消耗掉的尝试次数，如果输入的字母是正确的则不消耗尝试次数（即为0）。例如要猜的单词是google，初始猜测结果是6个下划线`______`，如果用输入`o`，则结果更新为`_o____`
* `total`是用来记录失败尝试次数的`Circuit`
* `delayEcho`的作用是把输入延迟一步输出，每一步的输入，都会作为下一步的输出，第一步的输出由参数指定。假设输入是`[b,c,d]`，入参是`a`，则输出则是`[a,b,c,d]`。当游戏处于进行中时`end`初始为`True`，当用户猜出了单词或者耗尽了尝试次数则延迟一步把下一步的结果设置为`False`，这样可以保证最后一步的结果可以被输出。

下面我们逐一的实现各个辅助函数，首先是`getWord`:
```haskell

dictionary = ["book", "hoogle", "google", "hello"]

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator rng g = accum g $ \() g1 -> randomR rng g1


pickWord :: StdGen -> Circuit () String
pickWord g = proc () -> do
    idx <- generator (0, length dictionary - 1) g -< ()
    returnA -< dictionary !! idx

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

instance ArrowChoice Circuit where
    left orig@(Circuit cir) = Circuit $ \case 
        Left b -> let (cir', c) = cir b
                    in (left cir', Left c)
        Right d -> (left orig, Right d)


getWord :: StdGen -> Circuit () String
getWord g = proc () -> do
    firstTime <- oneShot -< ()
    let input = if firstTime then Left () else Right ()
    mPicked <- (pickWord g >>^ Just) ||| arr (const Nothing) -< input
    mWord <- accum' Nothing mplus -< mPicked
    returnA -< fromJust mWord
```
`getWord`又用到了几个辅助函数,`oneShot`,`pickWord`,`generator`:
* `oneShot`是一个输出`[True, False, False,...]`的`Circuit`，因为只需要第一次的时候从词典中随机选择一个单词
* `generator`用来生成随机数
* `pickWord`用来从词典中选择单词

解读一下`getWord`的内部逻辑：
* `firstTime <- oneShot -< ()`: 判断是不是第一个输入
*  `mPicked <- (pickWord g >>^ Just) ||| arr (const Nothing) -< input`: 如果是第一次输入，则随机挑选一个单词
* `mWord <- accum' Nothing mplus -< mPicked`: 当选择好一个单词之后，后续每一步都返回这个单词
* 整体来看`getWord`是一个输出恒定的`Circuit`，第一步选择一个单词之后，每一都返回同一个单词。

接下来看`updateGuess`:
```haskell
attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
              ++ replicate (attempts - hung) '#'
              ++ replicate hung ' '
              ++ "]"

updateGuess :: Circuit (String, Maybe Char) (String, Int)
updateGuess = accum (repeat '_') $ \(word, letter) guess -> 
    case letter of
        Just l -> let (guess', changed) = unzip $ runCircuit (updateOnHit l) $ zip word guess
                   in ((guess', if or changed then 0 else 1), guess')
        Nothing -> let guess' = take (length word) guess
                    in ((guess', 0), guess')
  where updateOnHit :: Char -> Circuit (Char, Char) (Char, Bool)
        updateOnHit l = accum False $ \(a, b) acc -> if acc
                                                       then ((b, False), acc)
                                                       else if a == l && b /= l
                                                              then ((a, True),  True)
                                                              else ((b, False), False)
```                                                       
`updateGuess`接收用户的输入的字母和原单词，当用户猜对时返回猜测结果和消耗的尝试次数，猜测结果初始时是多个下划线，猜对一个字母则将对应位置的下划线复原，猜错则消耗一次尝试次数。其中用到了一个辅助函数`updateOnHit`，它会逐个检查单词的位置，如果这个位置还没有复原并且用户猜对了，则将这个位置复原，否则就消耗一次尝试次数，它使用`accum`构建，其中`acc`的初始值为`False`，猜对时更新成`True`并且后续直接返回`(b, False)`而不再检查。

最后是`main`函数的实现：
```haskell
main :: IO ()
main = do
    g <- getStdGen
    interact $ unlines                      -- Concatenate lines out output
        . ("Welcome to Arrow Hangman":)     -- Prepend a greeting to the output
        . concatMap snd . takeWhile fst  -- Take the [String]s as long as the first element of the tuples is True
        . runCircuit (hangman g)          -- Process the input lazily
        . ("":)                             -- Act as if the user pressed ENTER once at the start
        . lines                             -- Split input into lines
```
其中:
* `lines`函数把用户的输入按行分隔形成列表
* `("":)`模拟用户直接按下回车，输出初始情况下的结果
* `runCircuit (hangman g)`执行游戏主逻辑
* `concatMap snd . takeWhile fst`获取游戏进行过程中的每一步输出，并把描述游戏过程的字符串拼接成列表
* `("Welcome to Arrow Hangman":)`拼接上游戏开始的输出
* `unlines`把列表用换行符拼接，以便在控制台输出
