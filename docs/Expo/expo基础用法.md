## 下载keystore相关文件

在expo.dev中找到Configuration -> Credentials -> Application Identifier -> Build Credentials -> Android keystore -> Download

## 从aab包中提取apk文件

```bash
bundletool build-apks --bundle=./application-xxxxxx.aab --output=xxxxx.apks --ks=@xxxx__xxxxx-keystore.bak.jks --ks-pass=pass:xxxxxxxxxx --ks-key-alias=xxxxxxxxxx --key-pass=pass:xxxxxxxxx --mode=universal
```