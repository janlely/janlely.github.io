上一节中serve函数的签名:
```haskell
serve :: HasServer api '[] => Proxy api -> Server api -> Application
```
其中有一个类型类`HasServer`，它的定义如下
```haskell
class HasServer api context where
  -- | The type of a server for this API, given a monad to run effects in.
  --
  -- Note that the result kind is @*@, so it is /not/ a monad transformer, unlike
  -- what the @T@ in the name might suggest.
  type ServerT api (m :: * -> *) :: *

  route ::
       Proxy api
    -> Context context
    -> Delayed env (Server api)
    -> Router env

  hoistServerWithContext
      :: Proxy api
      -> Proxy context
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n
```
HasServer用来将API的定义与实现转换成可以实际处理请求的Server。其中:
* `context` 是一个类型列表，里面包含了一些额外的信息或配置，这些信息或配置可能会影响到如何处理请求。例如，你可能会在上下文中包含认证信息、日志记录器或者其他任何服务运行时可能需要的额外数据。Context 类型是 servant 中的一个特殊类型，它是一个 heterogenous list，用来存储不同类型的值。
* `api` 是通过一系列类型组合起来的，这些类型用来描述你的 web 服务可以执行的操作，例如获取资源、发布数据、处理不同的请求方法和路径等。这些类型使用 servant 提供的类型组合器（如 :>, :<<, Get, Post 等）来构建。API 类型是编译时的抽象，它告诉 servant 如何解析和处理进来的 HTTP 请求。
* `route` 函数根据你的 API 类型和上下文信息构建一个路由器，这个路由器可以匹配请求并将它们分派到正确的处理程序。
* `hoistServerWithContext` 函数允许你将一个在 monad m 中运行的服务器提升（hoist）到另一个 monad n 中。这在你想要改变你的服务器运行的上下文或副作用时非常有用。例如，你可以使用它来添加日志记录或异常处理等。

HasServer中使用了关联类型`ServerT`，在类型类中可以定义与类型参数关联的函数，同样也可以定义与类型参数关联和类型。在HasServer中`ServerT api (m :: * -> *)`是一个`*`，它表示一个具体类型，但这个类型又与`api`对应的类型相关。


关于Context:
* HasServer中的Context是一个heterogenous list(异构列表)
* 每一个api端点可以定义一个上下文类型，当多个api组合之后Servant类型系统会构建一个heterogenous list来表示Context，例如:
    ```haskell
    type CombinedAPI = API1 :<|> API2

    combinedServer :: Server CombinedAPI
    combinedServer = server1 :<|> server2

    context :: Context '[Context1, Context2]
    context = context1Value :. context2Value :. EmptyContext

    main :: IO ()
    main = run 8080 $ serveWithContext (Proxy :: Proxy CombinedAPI) context combinedServer
    ```
* 使用(:.)函数连接所要提供的上下文，Context中的类型的顺序不重要，Servant会自动选择正确的上下文给到api的处理逻辑。
* 使用`serveWithContext`函数来创建Application
