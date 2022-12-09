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
COPY toolbox/activate.r /root/toolbox/activate.r
COPY toolbox/aws/aws_credentials.r /root/toolbox/aws/aws_credentials.r
COPY toolbox/aws/client.r /root/toolbox/aws/client.r
COPY toolbox/aws/costs.r /root/toolbox/aws/costs.r
COPY toolbox/aws/current_servers.r /root/toolbox/aws/current_servers.r
COPY toolbox/aws/ec2.r /root/toolbox/aws/ec2.r
COPY toolbox/aws/instance.r /root/toolbox/aws/instance.r
COPY toolbox/aws/key_file.r /root/toolbox/aws/key_file.r
COPY toolbox/aws/key_pairs.r /root/toolbox/aws/key_pairs.r
COPY toolbox/aws/messaging.r /root/toolbox/aws/messaging.r
COPY toolbox/aws/rbox_init.r /root/toolbox/aws/rbox_init.r
COPY toolbox/aws/s3.r /root/toolbox/aws/s3.r
COPY toolbox/aws/security_groups.r /root/toolbox/aws/security_groups.r
COPY toolbox/ci/build.r /root/toolbox/ci/build.r
COPY toolbox/ci/dockerfile.r /root/toolbox/ci/dockerfile.r
COPY toolbox/ci/git.r /root/toolbox/ci/git.r
COPY toolbox/ci/init.r /root/toolbox/ci/init.r
COPY toolbox/onload/interactive.r /root/toolbox/onload/interactive.r
COPY toolbox/onload/options/default.r /root/toolbox/onload/options/default.r
COPY toolbox/onload/options/onload_app.r /root/toolbox/onload/options/onload_app.r
COPY toolbox/onload/options/onload_gert.r /root/toolbox/onload/options/onload_gert.r
COPY toolbox/onload/options/onload_renv.r /root/toolbox/onload/options/onload_renv.r
COPY toolbox/onload/options/onload_shiny.r /root/toolbox/onload/options/onload_shiny.r
COPY toolbox/onload/options/onload_tibble.r /root/toolbox/onload/options/onload_tibble.r
COPY toolbox/onload/user.r /root/toolbox/onload/user.r
COPY toolbox/templates/app/app.r /root/toolbox/templates/app/app.r
COPY toolbox/templates/app/src/modules/body.r /root/toolbox/templates/app/src/modules/body.r
COPY toolbox/templates/app/src/modules/cookies/cookies.r /root/toolbox/templates/app/src/modules/cookies/cookies.r
COPY toolbox/templates/app/src/modules/frontend.r /root/toolbox/templates/app/src/modules/frontend.r
COPY toolbox/templates/app/src/modules/head.r /root/toolbox/templates/app/src/modules/head.r
COPY toolbox/templates/app/src/modules/html.r /root/toolbox/templates/app/src/modules/html.r
COPY toolbox/templates/app/src/modules/oauth/oauth20.r /root/toolbox/templates/app/src/modules/oauth/oauth20.r
COPY toolbox/templates/app/src/modules/terminal/terminal.r /root/toolbox/templates/app/src/modules/terminal/terminal.r
COPY toolbox/templates/app/src/server.r /root/toolbox/templates/app/src/server.r
COPY toolbox/templates/app/src/ui.r /root/toolbox/templates/app/src/ui.r
COPY toolbox/templates/app-basic/app.r /root/toolbox/templates/app-basic/app.r
COPY toolbox/templates/app-basic/src/modules/body.r /root/toolbox/templates/app-basic/src/modules/body.r
COPY toolbox/templates/app-basic/src/modules/frontend.r /root/toolbox/templates/app-basic/src/modules/frontend.r
COPY toolbox/templates/app-basic/src/modules/head.r /root/toolbox/templates/app-basic/src/modules/head.r
COPY toolbox/templates/app-basic/src/modules/html.r /root/toolbox/templates/app-basic/src/modules/html.r
COPY toolbox/templates/app-basic/src/server.r /root/toolbox/templates/app-basic/src/server.r
COPY toolbox/templates/app-basic/src/ui.r /root/toolbox/templates/app-basic/src/ui.r
COPY toolbox/templates/plumber/plumber.r /root/toolbox/templates/plumber/plumber.r
COPY toolbox/utilities/send_email.r /root/toolbox/utilities/send_email.r
EXPOSE 8000