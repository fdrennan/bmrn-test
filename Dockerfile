FROM rust:1.58-buster

RUN echo "deb https://cloud.r-project.org/bin/linux/debian buster-cran40/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7'
#RUN rustup update
#RUN cargo install diesel_cli
#RUN apt update -qq -y
#RUN apt install -y software-properties-common dirmngr libcurl4-openssl-dev libssl-dev libssh2-1-dev \
#    libxml2-dev zlib1g-dev make git-core libcurl4-openssl-dev \
#    libxml2-dev libpq-dev cmake r-base r-base-dev libsodium-dev libsasl2-dev \
#    libharfbuzz-dev libfribidi-dev
#
#
#RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
#RUN unzip awscliv2.zip
#RUN ./aws/install
#
#WORKDIR /app
#COPY renv.lock /app/renv.lock
#COPY .Rprofile.docker .Rprofile
#COPY renv/activate.R renv/activate.R
#RUN R -e "renv::restore()"
#RUN R -e "install.packages('devtools')"
#RUN R -e "install.packages('box')"
#RUN R -e "reticulate::install_miniconda(force=TRUE)"

#WORKDIR /app/redpul
#COPY redpul/DESCRIPTION .
#COPY redpul/NAMESPACE .
#COPY redpul/R R
#COPY redpul/src src
#RUN mv /app/redpul/src/Makevars.docker /app/redpul/src/Makevars
#
#
#WORKDIR /app
#RUN R -e "devtools::install_deps('./redpul')"
#RUN R -e "devtools::document('./redpul')"
#RUN R -e "devtools::install('./redpul')"
#
#COPY functions functions
