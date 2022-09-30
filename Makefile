sass: style
	scss www/sass/styles.scss www/styles.css

style:
	R -e "styler::style_dir()"

.PHONY: redpul

redpul:
	R -e "devtools::install_deps('./redpul')"
	R -e "devtools::document('./redpul')"
	R -e "devtools::install('./redpul')"

push:
	git add --all
	git commit -m  " *  Author: $$(whoami)\n *  Created on: $$(date +%D)"
	git push origin $$(git rev-parse --abbrev-ref HEAD)