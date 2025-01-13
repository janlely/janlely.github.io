## 下载keystore相关文件

在expo.dev中找到Configuration -> Credentials -> Application Identifier -> Build Credentials -> Android keystore -> Download

## 从aab包中提取apk文件

```bash
bundletool build-apks --bundle=./application-a1f4a3e3-282b-44cb-83fa-3f4cd0022d70.aab --output=whisper.apks --ks=@janlely__whisper-keystore.bak.jks --ks-pass=pass:35c9a08af4146e78124b5a545d5d5080 --ks-key-alias=f0780f3ce6a2092d0b95133feedfa5c3 --key-pass=pass:a07982fc9eb36e7e9d1f5d55b68891fe --mode=universal
```