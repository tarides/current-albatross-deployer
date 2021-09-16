#!/bin/sh
CURRENT_DEPLOYER_USER=current-deployer

groupadd -g 496 $CURRENT_DEPLOYER_USER
useradd -g 496 -u 496 -d /nonexistent -s /usr/sbin/nologin $CURRENT_DEPLOYER_USER

sudo cp ../../_build/install/default/bin/current-deployerd /usr/local/sbin/
sudo cp ./current_deployerd.service ./current_deployerd.socket /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl stop current_deployerd
sudo systemctl start current_deployerd
