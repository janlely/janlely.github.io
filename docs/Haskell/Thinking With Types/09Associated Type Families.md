## 如何实现一个Type Safe的printf函数
c语言中的`printf`是一个类型不安全的函数，即使我们写如下的代码，在编译期也不会报错:
```c
printf("hello %d", "world");
```
如何借助于Haskell的类型系统来设计一个类型安全的`printf`呢?

## 类型安全的printf是什么样的？
`printf("hello %s, %d", "world", 123)`这样的`printf`语句对应的类型安全的`printf`是什么样的？

我们希望把`"hello %s, %d"`这一格式化的字符串表达成一个这样的Type:

<center>"hello " :<< String :<< ", " :<< Int</center>
(:<<)是一个类型构造函数。然后，我们希望可以这个Type转换成一个函数:

<center>String -> Int -> String</center>
有了这个函数，编译器就可以做类型检查了，`"hello %s, %d"`就只能接收一个`String`和一个`Int`。

## 实现过程
* 类型构造器(:<<)的定义
    ```haskell
    data (a :: k1) :<< (b :: k2)
    infixr 5 :<<
    ```
    定义(:<<)类型构造器，由于只在类型级别使用，所以不需要定义值构造器。

* 利用associated type family把类型表达成函数
    ```haskell
    class HasPrintf a where
      type Printf a :: Type
    ```
    这里的`a`是一个用(:<<)构造的类型，`Printf a`是一个函数

* 使用Proxy把Type降到Term，以便进行函数运算
    ```haskell
    class HasPrintf a where
      type Printf a :: Type
      format :: String -> Proxy a -> Printf a
    ```
    format函数把`String`和`Proxy a`转换成一个函数，这个函数的类型由a的函数确定。

* 定义printf函数
    ```haskell
    printf :: HasPrintf a => Proxy a -> Printf a
    printf = format ""
    ```

## 补充HasPrintf的实现细节
```haskell
class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

```

## 效果
```bashe
ghci> printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2
"1+2=3"
```
