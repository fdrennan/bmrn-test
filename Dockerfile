FROM rocker/verse:4.2

RUN apt update -qq -y

RUN apt install -y software-properties-common dirmngr libcurl4-openssl-dev \
    libssl-dev libssh2-1-dev libxml2-dev zlib1g-dev make git-core \
    libcurl4-openssl-dev libxml2-dev libpq-dev cmake \
    r-base r-base-dev libsodium-dev libsasl2-dev

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
RUN unzip awscliv2.zip
RUN ./aws/install

RUN R -e "install.packages(c('renv','rextendr', 'devtools', 'shiny', 'roxygen2', 'usethis', 'testthat', 'tidyverse'))"
RUN R -e "install.packages('reticulate')"
RUN R -e "reticulate::install_miniconda()"
ENV LC_ALL C.UTF-8
ENV LANG C.UTF-8

WORKDIR /app/
COPY DESCRIPTION .
COPY R R
COPY data data
COPY inputs inputs
#RUN R -e "install.packages(c('ggpubr', 'DT'))"
RUN R -e "devtools::install_deps('.')"
RUN R -e "devtools::document('.')"
RUN R -e "devtools::install('.')"

COPY word_style.docx .
COPY bib.bib .
COPY test_logo.svg test_logo.svg
COPY Test_Report.Rmd /app/Test_Report.Rmd
COPY app.R /app/app.R
COPY .env.container /app/.env
COPY .Rprofile /app/.Rprofile
COPY www /app/www
