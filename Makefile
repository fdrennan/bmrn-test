.PHONY: redpul aws build

push: updatenginxconf sass ndexr2s3
	git add --all
	git commit -m  " *  Author: $$(whoami)  *  Created on: $$(date)"
	echo "Pushing to $(BRANCH)"
	git push origin $(BRANCH)


### DEVELOPMENT
BRANCH := $(shell git for-each-ref --format='%(objectname) %(refname:short)' refs/heads | awk "/^$$(git rev-parse HEAD)/ {print \$$2}")
HASH := $(shell git rev-parse HEAD)W

ndexr2s3:
	aws s3 cp ./Makefile s3://ndexrapp/Makefile
	aws s3 cp ./docker-compose-prod.yaml s3://ndexrapp/docker-compose-prod.yaml
	aws s3 cp ./nginx.conf s3://ndexrapp/nginx.conf
	aws s3 cp ec2.nginx.conf s3://ndexrapp/ec2.nginx.conf

ec2init:
	R -e "box::use(./functions/aws[ec2_instance_create]);ec2_instance_create()"

ec2destroy:
	R -e "box::use(./functions/aws[ec2_instance_destroy])"

ndexrapp:
	aws s3 cp s3://ndexrapp ./ --recursive

sass: style
	scss www/sass/styles.scss www/styles.css

style:
	R -e "styler::style_dir()"

redpul:
	R -e "devtools::install_deps('./redpul')"
	R -e "devtools::document('./redpul')"
	R -e "devtools::install('./redpul')"

updatenginxconf:
	R -e "box::use(./functions/nginx/nginx[update_conf]);update_conf()"
	sudo cp localhost.nginx.conf /etc/nginx/nginx.conf


### END DEVELOPMENT

### SERVICES
aws:
	docker run --rm -ti -v ~/.aws:/root/.aws amazon/aws-cli s3 ls

sendmessage:
	aws sns publish --phone-number +12549318313 --message "Hello"

### END SERVICES

### DOCKER
describe:
	docker images ls
	docker volume ls

build: describe
	docker-compose build --parallel

latest:
	docker-compose --file docker-compose-gitlab.yaml up

prune:
	docker system prune -a
	docker volumn prune

bash:
	docker run -it ndexr_app bash

exec:
	docker exec -it ndexr_app bash

login:
	docker login -u drennanfreddy@gmail.com -p AeliaJames2022! registry.gitlab.com

up:
	docker-compose up -d
	docker-compose logs -f --tail=100
### END DOCKER


### GITLAB
# https://docs.gitlab.com/runner/install/docker.html
gitlabup:
	docker run --rm -t -i gitlab/gitlab-runner --help

gitlabsetup:
	docker volume create gitlab-runner-config

gitlab:
	docker run -d --name gitlab-runner --restart always \
		    -v /var/run/docker.sock:/var/run/docker.sock \
		        -v gitlab-runner-config:/etc/gitlab-runner \
			    gitlab/gitlab-runner:latest
gitlabauth:
	sudo gitlab-runner register --non-interactive --executor docker+machine --docker-image docker:latest --url https://gitlab.com/ --registration-token GR1348941whxCEwiTrhz8udmhej1p

runnerinstall: login
	curl -LJO "https://gitlab-runner-downloads.s3.amazonaws.com/latest/deb/gitlab-runner_${arch}.deb"
	dpkg -i gitlab-runner_ubuntu.deb
	sudo gitlab-runner register


### END GITLAB
focal: login
	curl -LJO "https://gitlab-runner-downloads.s3.amazonaws.com/latest/deb/gitlab-runner_focal.deb"
	dpkg -i gitlab-runner_ubuntu.deb
	sudo gitlab-runner register

jammy: login
	curl -LJO "https://gitlab-runner-downloads.s3.amazonaws.com/latest/deb/gitlab-runner_jammy.deb"
	dpkg -i gitlab-runner_ubuntu.deb
	sudo gitlab-runner register


shiny:
	docker compose --file docker-compose-shiny.yaml up

