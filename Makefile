sass: style
	scss www/sass/styles.scss www/styles.css

style:
	R -e "styler::style_dir()"

.PHONY: redpul

redpul:
	R -e "devtools::install_deps('./redpul')"
	R -e "devtools::document('./redpul')"
	R -e "devtools::install('./redpul')"

BRANCH := $(shell git for-each-ref --format='%(objectname) %(refname:short)' refs/heads | awk "/^$$(git rev-parse HEAD)/ {print \$$2}")
HASH := $(shell git rev-parse HEAD)

push:
	git add --all
	git commit -m  " *  Author: $$(whoami)  *  Created on: $$(date)"
	git push origin $(BRANCH)

	#git push origin $$(git rev-parse --abbrev-ref HEAD)