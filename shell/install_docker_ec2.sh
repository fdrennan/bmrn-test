#!/bin/bash
# Install docker
LOGFILE='/ndexrinstall.log'
savelog "$LOGFILE"
exec &> >(tee "$LOGFILE")
echo "alias tlog=\"tail -f /ndexrinstall.log\"" >> /home/ubuntu/.bashrc

echo "#!/bin/bash" >> /root/.start
echo "aws s3 cp s3://ndexrapp /home/ubuntu --recursive" >> /root/.start
echo "mv /home/ubuntu/ec2.nginx.conf /etc/nginx/nginx.conf" >> /root/.start
echo "systemctl restart nginx" >> /root/.start

echo "#!/bin/bash" >> /home/ubuntu/.start
echo "make login" >> /ubuntu/.start
echo "docker compose up -d" >> /home/ubuntu/.start
echo "echo \"###################################\"" >> /home/ubuntu/.bashrc
echo "echo \"LOOOOOOK HERE AT THESE INSTRUCTIONS\"" >> /home/ubuntu/.bashrc
echo "echo \"login as root: sudo su -" >> /home/ubuntu/.bashrc
echo "echo \"then execute . /root/.start\" to initialize" >> /home/ubuntu/.bashrc
echo "echo \"login as root and execute tlog to view logs from ec2 user_data" >> /home/ubuntu/.bashrc
echo "###################################"


deb https://nginx.org/packages/ubuntu/ jammy nginx
deb-src https://nginx.org/packages/ubuntu/ jammy nginx

apt-get update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common gnupg lsb-release unzip \
  make nginx

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
mkdir /root/.aws

echo "[default]
region = us-east-2
output = json" >> /root/.aws/config
echo "[default]
aws_access_key_id = AKIAWEUHS5MEZ24ZVER6
aws_secret_access_key = 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" >> /root/.aws/credentials

