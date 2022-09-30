sass: style
	scss www/sass/styles.scss www/styles.css

style:
	R -e "styler::style_dir()"

.PHONY: redpul

redpul:
	R -e "devtools::install_deps('./redpul')"
	R -e "devtools::install('./redpul')"

