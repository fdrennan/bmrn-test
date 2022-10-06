#!/bin/bash
# Install docker
LOGFILE='/ndexrinstall.log'
savelog "$LOGFILE"
exec &> >(tee "$LOGFILE")

apt-get update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common gnupg lsb-release unzip \
  make

mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
apt-get update
apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin
DOCKER_CONFIG=${DOCKER_CONFIG:-$HOME/.docker}
usermod -aG docker ubuntu

curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
./aws/install

echo "[default]\nregion = us-east-2\noutput = json" >> /home/ubuntu/.aws/config
echo "[default]\naws_access_key_id = AKIAWEUHS5MEZ24ZVER6\naws_secret_access_key = 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" >> /home/ubuntu/.aws/credentials
echo "aws s3 cp s3://ndexrapp ./ --recursive" >> /home/ubuntu/.bashrc
