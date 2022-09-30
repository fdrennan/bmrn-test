FROM rust:1.58-buster
ENV REDPUL_VERSION=${REDPUL_VERSION}
RUN echo "deb https://cloud.r-project.org/bin/linux/debian buster-cran40/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7'
RUN rustup update
RUN cargo install diesel_cli
RUN apt update -qq -y
RUN apt install -y software-properties-common dirmngr libcurl4-openssl-dev libssl-dev libssh2-1-dev \
    libxml2-dev zlib1g-dev make git-core libcurl4-openssl-dev \
    libxml2-dev libpq-dev cmake r-base r-base-dev libsodium-dev libsasl2-dev

RUN R -e "install.packages(c('renv', 'rextendr', 'devtools', 'shiny', 'box))"
RUN R -e "install.packages(c('renv', 'rextendr', 'devtools', 'shiny', 'roxygen2', 'usethis', 'testthat', 'tidyverse'))"


WORKDIR /redpul/redpul
COPY redpul/DESCRIPTION .
COPY redpul/NAMESPACE .
COPY redpul/R R
COPY redpul/src src
COPY redpul/.config/Makevars ./src/Makevars
COPY redpul/main.R .
COPY redpul/plumber.R .
COPY redpul/pulse.R .

WORKDIR /redpul
RUN R -e "devtools::install_deps('./redpul')"
RUN R -e "rextendr::document('./redpul')"
RUN R -e "devtools::document('./redpul')"
RUN R -e "devtools::install('./redpul')"

WORKDIR /redpul/frontend
COPY ./frontend/DESCRIPTION .
COPY ./frontend/NAMESPACE .
COPY ./frontend/R R

WORKDIR /redpul
RUN R -e "devtools::install_deps('./frontend')"
RUN R -e "devtools::document('./frontend')"
RUN R -e "devtools::install('./frontend')"

COPY ./gmail_creds /redpul/gmail_creds
