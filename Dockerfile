FROM registry.gitlab.com/fdrennan/ndexr:base

COPY functions functions
COPY run.r .
COPY .aws /root/.aws
COPY .Renviron .
COPY node_modules node_modules
COPY www www
