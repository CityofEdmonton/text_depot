# syntax = docker/dockerfile:1.0-experimental # https://docs.docker.com/develop/develop-images/build_enhancements/
FROM rocker/r-ver:4.0.3

RUN export DEBIAN_FRONTEND=noninteractive && apt-get -y update \
  && apt-get install -y \
  alien \
  bzip2 \
  cmake \
  curl \
  file \
  gdal-bin \
  gnupg2 \
  libaio1 \
  libapparmor1 \
  libcairo2 \
  libcairo2-dev \
  libcurl4-openssl-dev \
  libedit2 \
  libgdal-dev \
  libglpk-dev \
  libpoppler-cpp-dev \
  libproj-dev \
  libsqliteodbc \
  libssl-dev \
  libudunits2-dev \
  libxml2-dev \
  libxt-dev \
  libxt6 \
  lsb-release \
  odbc-postgresql \
  openjdk-8-jdk \
  openssh-client \
  pandoc \
  pandoc-citeproc \
  postgresql \
  procps \
  psmisc \
  r-cran-cairo \
  swaks \
  tcl-dev \
  tk-dev \
  unixodbc \
  zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

# get from https://packagemanager.rstudio.com/client/#/repos/1/overview
# Freezing packages to April 22, 2021:
RUN echo "options(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/all/__linux__/focal/2511902'))" >> $R_HOME/etc/Rprofile.site

RUN R -e "install.packages(c('assertthat', \
  'data.table', \
  'dplyr', \
  'DT', \
  'elastic', \
  'future', \
  'future.callr', \
  'ggplot2', \
  'ggthemes', \
  'httr', \
  'jsonlite', \
  'leaflet', \
  'lubridate', \
  'memoise', \
  'plotly', \
  'promises', \
  'rmarkdown', \
  'rgdal', \
  'shiny', \
  'shinycssloaders', \
  'shinydashboard', \
  'shinyjs', \
  'shinyWidgets', \
  'sf', \
  'stringr', \
  'timetk', \
  'htmltools'))"

# Add certs for accessing elastic search servers that require them
COPY elasticsearch/certificates/*.crt /usr/local/share/ca-certificates/
RUN update-ca-certificates

RUN mkdir /shinyapp
WORKDIR /shinyapp/
ADD ./ ./

RUN useradd shiny -u 5000 -m -b /home
RUN chown -R shiny:shiny /shinyapp
USER shiny

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/shinyapp', host = '0.0.0.0', port = 3838)"]
