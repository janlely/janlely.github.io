这一节我们开始着手使用`Circuit`开发Hangman游戏。Hangman游戏游戏的规则是：
* 开始时，程序会随机从词典表中选择一个单词由玩家来猜，程序每一步会输出玩家猜测一次之后的结果，初始结果是由多个下划线连成的与单词长度相等的占位字符串
* 当玩家每输入一个字母时，如果字母与单词中某个下划线对应的中一个字母匹配，则该位置的字母会被复原，否则玩家会消耗一次尝试的机会
* 玩家完整的猜对单词视为胜利
* 玩家的尝试次数消耗殆尽视为失败

从主函数`hangman`开始，不难想到`hangman`函数和签名:
```haskell
hangman :: StdGen -> Circuit String (Bool, [String])
```
其中：
* `StdGen`是随机数种子，用来从词典中随机选择一个单词
* `String`表示用记的输入，通常是一个字母，如果用户输入多个字母则只会取第一个
* `Bool`用来表示游戏是否已结束
* `[String]`包含了游戏过程中程序的输出，包含了当前的单词、剩余的尝试次数、输赢的结束语


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
someFunc :: IO ()
someFunc = do
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

最后附上完整的代码:
```haskell
{-# LANGUAGE Arrows #-}
module Hangman(
    someFunc
) where

import Control.Arrow ((>>^), returnA, Arrow(arr), ArrowChoice(..))
import System.Random (Random, StdGen, randomR, getStdGen)
import Control.Monad (mplus)
import Data.Maybe (fromJust, listToMaybe)
import Circuit





dictionary :: [String]
dictionary = ["hello", "world", "haskell", "functional", "programming"]

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator rng g = accum g $ \_ acc -> randomR rng acc

pickWord :: StdGen -> Circuit () String
pickWord g = proc () -> do
    idx <- generator (0, length dictionary - 1) g -< ()
    returnA -< dictionary !! idx

oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)


getWord :: StdGen -> Circuit () String
getWord g = proc () -> do
    firstTime <- oneShot -< ()
    let input = if firstTime then Left () else Right ()
    mPicked <- (pickWord g >>^ Just) ||| arr (const Nothing) -< input
    mWord <- accum' Nothing mplus -< mPicked
    returnA -< fromJust mWord

delayEcho :: a -> Circuit a a
delayEcho acc = accum acc (flip (,))

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


attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
              ++ replicate (attempts - hung) '#'
              ++ replicate hung ' '
              ++ "]"

someFunc :: IO ()
someFunc = do
    g <- getStdGen
    interact $ unlines                      -- Concatenate lines out output
        . ("Welcome to Arrow Hangman":)     -- Prepend a greeting to the output
        . concatMap snd . takeWhile fst  -- Take the [String]s as long as the first element of the tuples is True
        . runCircuit (hangman g)          -- Process the input lazily
        . ("":)                             -- Act as if the user pressed ENTER once at the start
        . lines                             -- Split input into lines
```
