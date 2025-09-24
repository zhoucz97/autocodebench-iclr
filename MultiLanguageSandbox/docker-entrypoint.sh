#!/bin/bash
set -e

echo "PATH is: $PATH"

# 允许已建立的连接和本地回环接口
iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
iptables -A INPUT -i lo -j ACCEPT
iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
iptables -A OUTPUT -o lo -j ACCEPT

# 默认拒绝所有其他出入站流量
iptables -P INPUT DROP
iptables -P OUTPUT DROP

# 允许服务的入包以及回包（8080需要改为服务实际使用端口）
iptables -A INPUT -p tcp --dport 8080 -j ACCEPT
iptables -A OUTPUT -m conntrack --ctstate ESTABLISHED,RELATED -p tcp --ctorigdstport 8080 -j ACCEPT

# 允许 DNS 查询（UDP 53）
iptables -A OUTPUT -p udp --dport 53 -j ACCEPT

# 允许访问 mirrors.tencent.com 的 443 端口
IPS=$(dig +short mirrors.tencent.com | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$')
for IP in $IPS; do
    iptables -A OUTPUT -p tcp -d $IP --dport 443 -j ACCEPT
done

# 允许访问日志汇，用于agent下发配置并采集日志
# 下发配置
IPS=$(dig +short log-etcd-idc.zhiyan.tencent-cloud.net | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$')
for IP in $IPS; do
    iptables -A OUTPUT -p tcp -d $IP --dport 2379 -j ACCEPT
done
# 采集上报日志
iptables -A OUTPUT -p tcp --dport 12001 -j ACCEPT

# 允许访问Clickhouse上报统计数据
# test
iptables -A OUTPUT -p tcp -d 11.145.84.136 --dport 9000 -j ACCEPT
# prod
iptables -A OUTPUT -p tcp -d 11.145.84.130 --dport 9000 -j ACCEPT

# 启动 Gunicorn（前台主进程）
exec gunicorn -c /data/gunicorn_config.py \
  -w 16 --threads 1 \
  -b 0.0.0.0:8080 \
  sandbox:app