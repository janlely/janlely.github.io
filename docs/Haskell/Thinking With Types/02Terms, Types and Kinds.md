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


data Unit = Unit
--定义了一个Kind是Unit，它有一个Type是'Unit

-- 正常编译
Maybe Unit

-- 编译报错，因为Maybe的Kind是 TYPE -> TYPE，而'Unit不属于TYPE,它属于Unit
Maybe 'Unit
```


## Promotion of Built-In Types
导入GHC.TypeLits模块之后就可以在type-level使用(strings, numbers, lists and tuples)这些Built-in的数据类型了
### Symbol
String类型在Type Level对应的是Symbol
```haskell
ghci> import GHC.TypeLits
ghci> :set -XDataKinds

ghci> :kind "hello"
"hello" :: Symbol

ghci> :kind AppendSymbol
AppendSymbol :: Symbol -> Symbol -> Symbol
```

## Natural Numbers
Number类型在Type Level对应的是Nat
```haskell
ghci> :set -XTypeOperators -XNoStarIsType -XDataKinds

ghci> :kind 5085072209
5085072209 :: Natural

-- 整数
:kind! (1 + 17) * 3 
(1 + 17) * 3 :: Natural
= 54

:kind! 1 * 2
1 * 2 :: Natural
= 2

:kind! (Div 128 8) ^ 2
(Div 128 8) ^ 2 :: Natural
= 256


-- 列表List
:k [Bool]
[Bool] :: Type

:k '[Bool]
'[Bool] :: [Type]

:k '[ 'True ]
'[ 'True ] :: [Bool]

-- Tuple
:kind '(2, "tuple")
'(2, "tuple") :: (Natural, Symbol)
```

## Type Level Functions
不能像定义term级别的函数一样定义type级别的函数，但是可以使用-XTypeFamilies来实现
```haskell
type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

:kind Or
Or :: Bool -> Bool -> Bool
```

### Exercise 2.4-i
```haskell
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not _ = 'True
```
Type-Level Functions不能部分应用，例如下面的写法会报错:
```haskell
type family Map (x :: a -> b) (i :: [a]) :: [b] where 
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

:t undefined :: Proxy (Map (Or 'True) '[ 'True, 'False, 'False '])

error: The type family ‘’Or should have 2 arguments,
            but has been given 1
```
因为Or作为一个Type-Level Function必须接收两个类型参数，不能像Term-Level Function那样进行"部分应用"。

Type Families中的类型定义不能像函数定义那样来写，例如
```haskell
type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y
  
--不能写成
type family Or :: Bool -> Bool -> Bool where 
  Or 'True y = 'True
  Or 'False y = y
  
type family Foo (x :: Bool) (y :: Bool) :: Bool  
:kind Foo
Foo :: Bool -> Bool -> Bool

type family Bar x y :: Bool -> Bool -> Bool
:kind Bar
Bar :: TYPE -> TYPE -> (Bool -> Bool -> Bool)

```
closed type families可以被视为Type-Level Functions