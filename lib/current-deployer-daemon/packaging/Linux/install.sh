#!/bin/sh
CURRENT_DEPLOYER_USER=current-deployer

groupadd -g 497 $CURRENT_DEPLOYER_USER
useradd -g 497 -u 497 -d /nonexistent -s /usr/sbin/nologin $CURRENT_DEPLOYER_USER
sudo mkdir -m 0700 -p /var/lib/current-deployer/

sudo cp ../../_build/install/default/bin/current-deployerd /usr/local/sbin/
sudo cp ./current_deployerd.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl stop current_deployerd
sudo systemctl start current_deployerd
