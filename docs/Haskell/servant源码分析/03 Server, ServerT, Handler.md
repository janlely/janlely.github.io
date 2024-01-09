## ServerT
`ServerT`是定义在HasServer中的关联类型:
```haskell
class HasServer api context where

  type ServerT api (m :: * -> *) :: *
  ...
```
它有两个参数：
* api 是使用servant的api组合子构建的API
* m 是一个Monad

  如果定义一个api:
  ```haskell
  type MyAPI = Get '[JSON] [MyData]
  ```
  那么:
  ```haskell
  ServerT MyAPI m = m [MyData]
  ```

## Server
`Server`是一个类型别名:
```haskell
type Server api = ServerT api Handler
```
使用`Server api`的地方都可以使用`ServerT api Handler`代替。而`ServerT api Handler`也是一个类型别名，它就是`Handler ？`，在之前的`MyAPI`这个例子中就是`Handler [MyData]`

## Handler
Handler是一个Monad，其中定义了核心请求的处理逻辑:
```haskell
newtype Handler a = Handler { runHandler' :: ExceptT ServerError IO a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, Generic
    , MonadError ServerError
    , MonadThrow, MonadCatch, MonadMask
    )

```
其中使用了`ExceptT`这个Monad transformer，它的定义是:
```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
```
Handler运行之后要么返回ServerError，要么返回一个正确的响应。

Handler实现了MonadIO，因此可以使用liftIO把IO提升到Handler。例如下面的代:
```haskell
type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)
```
`IOAPI1`是一个返回文件内容的API，使用`liftIO`函数可以把一个文件IO提升到Handler。