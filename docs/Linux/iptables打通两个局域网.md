- 假设有三台机器A，B，C。A和B在一个局域网内(172.16.1.0/24)，B和C在一个局域网内(172.17.1.0/24)。如何实现在A上直接访问C上的资源？  
- 首先，在A上添加一条路由规则，将所有到172.17.1.0/24的流量的网关设置成B  
- 然后在B上开启ip转发，并添加几条iptables规则  
``` shell
#在 /etc/sysctl.conf中添加net.ipv4.ip_forward = 1
sysctl -p

#添加iptables规则: 其中tun0是172.16.1.0/24的接口, eth0是172.17.1.0/24的接口
sudo iptables -A FORWARD -i tun0 -o eth0 -j ACCEPT
sudo iptables -A FORWARD -i eth0 -o tun0 -m state --state ESTABLISHED,RELATED -j ACCEPT
sudo iptables -t nat -A POSTROUTING -s 172.16.1.0/24 -d 172.7.1.0/24 -o eth0 -j MASQUERADE
```