FROM ubuntu:jammy
MAINTAINER Freddy Drennan <fdrennan@ndexr.com>
WORKDIR /ndexr/install
RUN apt-get update
RUN apt-get install -y gnupg2
RUN apt-get install -y software-properties-common
ENV "TERM"="xterm-256color"
ENV "DEBIAN_FRONTEND"="noninteractive"
ENV "TZ"="Etc/UTC"
RUN apt-get update
RUN apt-get install -y --no-install-recommends tzdata wget vim curl git-core lsb-release libssl-dev libgit2-dev libcurl4-openssl-dev libssh2-1-dev libsodium-dev libxml2-dev dirmngr libudunits2-dev libharfbuzz-dev libfribidi-dev zlib1g-dev libpq-dev libsasl2-dev cmake libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libcairo2-dev
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
RUN apt-get update
RUN apt-get install -y --no-install-recommends r-base r-base-dev
RUN R -e 'install.packages("renv", dependencies = TRUE)'
RUN R -e 'install.packages("shiny", dependencies = TRUE)'
RUN R -e 'install.packages("plumber", dependencies = TRUE)'
RUN R -e 'install.packages("tidyverse", dependencies = TRUE)'
RUN apt-get update
RUN apt-get install -y --no-install-recommends  default-jre  default-jdk
RUN R CMD javareconf
RUN R -e 'install.packages("reticulate")'
RUN R -e 'reticulate::install_miniconda(force = TRUE)'
RUN R -e 'reticulate::py_install("boto3")'
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
RUN unzip awscliv2.zip
RUN ./aws/install
RUN rm awscliv2.zip
RUN apt-get install -y zsh
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
WORKDIR /root
COPY renv.lock /root/renv.lock
RUN R -e 'renv::activate(getwd())'
RUN R -e 'renv::restore(getwd())'
COPY R/history/quick_plot.r /root/R/history/quick_plot.r
COPY R/main.r /root/R/main.r
EXPOSE 8000