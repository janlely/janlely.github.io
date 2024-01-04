servant服务的hello world:
```haskell
module Main where

import Data.Time.Calendar ( Day, fromGregorian )
import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON )
import Data.Aeson.Types ()
import Servant.Server (Server, Application, serve)
import Servant ((:>), Get, JSON, (:<|>), ReqBody)
import Data.Proxy (Proxy(Proxy))
-- import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )

main :: IO ()
main = run 8081 app

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]


server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
```

### run函数
从main函数中的run函数入手，run函数的签名是:
```haskell
run :: Port -> Application -> IO ()
```
其作用是把一个Application运行在一个端口上，再看Application的定义:
```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```
### Application类型
下面详细解释一下这个类型定义:
* Application是一个高阶函数，接收一个Request和一个(Response -> IO ResponseReceived)函数
* Request 封装了所有 HTTP 请求的详细信息，例如 URL、HTTP 方法、头信息等。
* (Response -> IO ResponseReceived) 它接受一个 Response 类型的值，并返回一个 IO ResponseReceived。Response 封装了将要发送回客户端的所有 HTTP 响应的详细信息，例如状态代码、响应头、响应体等。IO ResponseReceived 是一个 IO 操作，表明发送响应是一个有副作用的操作（即在外部世界中发送数据），ResponseReceived 是一个标记类型，用于指示响应已经被成功处理。
* IO ResponseReceived 表示的是一个 IO 操作，当执行这个操作时，会发送 HTTP 响应并返回一个 ResponseReceived 值，以指示响应已经发送。

Application的核心是处理Request并生成Response，然后利用传入的(Response -> IO ResponseReceived)函数来处理将响应发送给客户端的操作，利用高阶函数实现了解耦。


### serve函数
serve函数是目标是生成Application，它的签名是:
```haskell
serve :: HasServer api '[] => Proxy api -> Server api -> Application
```
其中：
* HasServer api '[]:
这是一个类型类约束。HasServer 类型类有两个参数：第一个参数 api 对应你定义的 API 类型；第二个参数是一个中间件列表，在这个上下文中它是空的 '[]。这个约束表明 api 必须有一个对应的服务器实现，即你可以为这个 API 类型构建一个 web 服务器。

* Proxy api:
Proxy是一个类型，它的目的是在运行时传递类型信息。由于 Haskell 是静态类型的，类型信息通常在编译时就已经丢失。当你需要在运行时引用某个类型时，可以使用Proxy 类型。在这里，Proxy api表示你要引用的 API 类型。这对serve` 函数来说是必要的，因为它需要知道应该为哪个 API 类型构建服务器。

* Server api:
Server api是另一个类型，它表示一个服务的实现。对于给定的api 类型，Server api是一个具体的 Haskell 值，它定义了如何处理 API 中的每个端点。例如，如果你的 API 有一个获取用户的端点，那么Server api` 就需要包含一个函数来处理这个端点的请求并返回相应的数据。

* Application:
这是函数的返回类型。正如之前解释的，Application 是一个类型别名，它定义了一个 WAI 网络应用程序。serve 函数返回的 Application 可以被任何 WAI 兼容的服务器，如 Warp，用来启动并运行你的 web 服务。

`HasServer api '[]`如何被满足呢？

每种 API 组合子，例如 Get, Post, :<|>, :> 等，都有一个 HasServer 实例，指明如果你拥有这一部分 API 的处理函数，你就可以构建出服务这个 API 的服务端代码。


### API组合子
使用servant提供的API组合子生成的api类型自动满足HasServer约束，例如hello world中的UserAPI：
```haskell
type UserAPI = "users" :> Get '[JSON] [User]
```
使用(:>)和Get定义了一个userAPI:
* path是 `/users`
* method是 `GET`
* '[JSON] 表示响应类型是JSON，使用列表是因为HTTP是可以支持多种响应类型的
* [User] 表示响应的数据类型是[User]，为可以最终以JSON形式返回，需要User可以转换成JSON，前面有一行代码
    ```haskell
    instance ToJSON User
    ```
    正是这个作用。

### 请求处理逻辑
`Server UserAPI`这个类型表示在UserAPI这个接口定义上的Server,其核心是如何生成User类型的结果。示例中使用了
```haskell
return users
```
这个最简单的实现，即没有任何处理逻辑，直接返回users数据。