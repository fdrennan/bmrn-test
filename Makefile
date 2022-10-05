.PHONY: redpul aws build

### DEVELOPMENT
BRANCH := $(shell git for-each-ref --format='%(objectname) %(refname:short)' refs/heads | awk "/^$$(git rev-parse HEAD)/ {print \$$2}")
HASH := $(shell git rev-parse HEAD)

sass: style
	scss www/sass/styles.scss www/styles.css

style:
	R -e "styler::style_dir()"

redpul:
	R -e "devtools::install_deps('./redpul')"
	R -e "devtools::document('./redpul')"
	R -e "devtools::install('./redpul')"

push: sass
	git add --all
	git commit -m  " *  Author: $$(whoami)  *  Created on: $$(date)"
	echo "Pushing to $(BRANCH)"
	git push origin $(BRANCH)
### END DEVELOPMENT

### SERVICES
aws:
	docker run --rm -ti -v ~/.aws:/root/.aws amazon/aws-cli s3 ls
### END SERVICES

### DOCKER
describe:
	docker images ls
	docker volume ls

build: describe
	docker-compose build --parallel

prune:
	docker system prune -a
	docker volumn prune

bash:
	docker run -it ndexr_app bash

exec:
	docker exec -it ndexr_app bash
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
	sudo gitlab-runner register --url https://gitlab.com/ --registration-token GR1348941whxCEwiTrhz8udmhej1p

### END GITLAB


