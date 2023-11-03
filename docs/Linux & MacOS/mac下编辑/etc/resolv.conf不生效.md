mac通过openvpn连了公司的内网，内网中有一个dns服务器，需要配置mac的dns解析。一个奇怪的问题是：当我更新了/etc/resolv.conf之后，nslookup和dig命令都ok，但是ping就不行。用下面的命令清除了dns缓存也不行
```bash
sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder
```
最后发现，手动更新/etc/resolv.conf文件不行，需要在mac的网络设置的界面上去添加DNS的配置。虽然最终也会更新/etc/resolv.conf，但是效果就是不一样。