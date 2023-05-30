# syntax = docker/dockerfile:1.0-experimental # https://docs.docker.com/develop/develop-images/build_enhancements/
FROM rocker/r-ver:4.2.1

RUN export DEBIAN_FRONTEND=noninteractive && apt-get -y update \
  && apt-get install -y \
  libgdal-dev \
  libudunits2-dev \
  libxt6 \
  xdg-utils \
  && rm -rf /var/lib/apt/lists/*


# Freezing packages:
ARG TARGETPLATFORM
RUN case ${TARGETPLATFORM} in \
  "linux/amd64") \
  echo "options(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/cran/__linux__/jammy/2022-04-22'))" >> $R_HOME/etc/Rprofile.site ;; \
  "linux/arm64") \
  echo "options(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/cran/2022-04-22'))" >> $R_HOME/etc/Rprofile.site ;; \
esac

RUN R -e "options(warn = 2); install.packages(c('assertthat', \
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
  'shiny', \
  'shinycssloaders', \
  'shinydashboard', \
  'shinyjs', \
  'shinyWidgets', \
  'sf', \
  'stringr', \
  'timetk', \
  'htmltools', \
  'xml2'))"

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
