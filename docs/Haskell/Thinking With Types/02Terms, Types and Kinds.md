## Terms, Types, Kinds
every **Term** has a **Type**: 
- 4是一个Term,它的Type是Int

every **Type** has a **Kind**: 
- Int是一个Type，它的Kind是 TYPE
- Maybe Bool是一个Type，Maybe的Kind是 TYPE -> TYPE
- Either的Kind是 TYPE -> TYPE -> TYPE
- MaybeT的Kind是(TYPE -> TYPE) -> TYPE -> TYPE
- Show的Kine是 TYPE -> Constraint

## -XDataKinds
开启-XDataKinds之后，定义Type的同时也会定义Kind
```haskell
data Bool = True | False
-- 上面定义同时会生成类似下面的定义，注意: 实际上不存在这样的语法
kind Bool = 'True | 'False 

ghci> :t True
True :: Bool

ghci> :k 'True
'True :: Bool
```


## Promotion of Built-In Types
导入GHC.TypeLits模块之后就可以在type-level使用(strings, numbers, lists and tuples)这些Built-in的数据类型了
### Symbol
```haskell
ghci> import GHC.TypeLits
ghci> :set -XDataKinds
ghci> :kind "hello"
"hello" :: Symbol
ghci>
```