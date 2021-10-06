#!/bin/sh
CURRENT_DAEMON_USER=current-iptables-daemon

groupadd -g 497 $CURRENT_DAEMON_USER
useradd -g 497 -u 497 -d /nonexistent -s /usr/sbin/nologin $CURRENT_DAEMON_USER

sudo cp ../../../../_build/install/default/bin/current-iptables-daemon /usr/local/sbin/
sudo cp ./current-iptables-daemon.service ./current-iptables-daemon.socket /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl stop current-iptables-daemon
sudo systemctl start current-iptables-daemon
