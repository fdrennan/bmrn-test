FROM registry.gitlab.com/fdrennan/ndexr:base

COPY functions functions
COPY .aws /root/.aws
COPY .Renviron .
COPY node_modules node_modules
COPY www www
COPY finances finances
