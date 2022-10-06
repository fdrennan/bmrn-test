#!/bin/bash
# Install docker
LOGFILE='/ndexrinstall.log'
savelog "$LOGFILE"
exec &> >(tee "$LOGFILE")

apt-get update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common gnupg lsb-release

mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
apt-get update
apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin
DOCKER_CONFIG=${DOCKER_CONFIG:-$HOME/.docker}
#mkdir -p $DOCKER_CONFIG/cli-plugins
#curl -SL https://github.com/docker/compose/releases/download/v2.11.2/docker-compose-linux-x86_64 -o $DOCKER_CONFIG/cli-plugins/docker-compose
#chmod +x $DOCKER_CONFIG/cli-plugins/docker-compose
usermod -aG docker ubuntu

# sudo usermod -aG docker $USER
