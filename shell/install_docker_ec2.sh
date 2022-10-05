#!/bin/bash
# Install docker
LOGFILE='/ndexrinstall.log'
savelog "$LOGFILE"
exec &> >(tee "$LOGFILE")

apt-get update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common gnupg lsb-release -y

mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-compose-plugin

usermod -aG docker $user
usermod -aG docker ubuntu

# sudo usermod -aG docker $USER
