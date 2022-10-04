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
	sleep 10
	docker volume ls
	sleep 10

build: describe
	docker-compose build --parallel

prune:
	docker system prune -a
	docker volumn prune

bash:
	docker run -it ndexr_app bash
### END DOCKER
