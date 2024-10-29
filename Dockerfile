# syntax = docker/dockerfile:1.0-experimental # https://docs.docker.com/develop/develop-images/build_enhancements/
FROM rocker/r-ver:4.3.0

RUN export DEBIAN_FRONTEND=noninteractive && apt-get -y update \
  && apt-get install -y \
  libgdal-dev \
  libudunits2-dev \
  libxt6 \
  xdg-utils \
  # following 3 libraries were needed when we switched to installing cran 
  # libraries from source (because arm binaries not available)
  # may be able to remove when arm cran binaries become available
  libharfbuzz-dev \
  libfribidi-dev \
  libgit2-dev \
  libfontconfig1-dev \ 
  && rm -rf /var/lib/apt/lists/*

# Freezing packages. Get available URLs at https://packagemanager.posit.co/client/#/repos/cran/setup
ARG PACKAGEDATE=2024-09-01
ARG TARGETPLATFORM
RUN case ${TARGETPLATFORM} in \
  "linux/amd64") \
  echo "options(repos = c(REPO_NAME = 'https://packagemanager.posit.co/cran/__linux__/jammy/${PACKAGEDATE}'))" >> $R_HOME/etc/Rprofile.site ;; \
  "linux/arm64") \
  echo "options(repos = c(REPO_NAME = 'https://packagemanager.posit.co/cran/${PACKAGEDATE}'))" >> $R_HOME/etc/Rprofile.site ;; \
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
  'xml2'), Ncpus = max(1L, parallel::detectCores()))"

# Add certs for accessing elastic search servers that require them
COPY elasticsearch/certificates/*.crt /usr/local/share/ca-certificates/
RUN update-ca-certificates

# There is an issue using SSL on AMR images on mac due to this: 
# https://stackoverflow.com/questions/75763525/curl-35-error0a000152ssl-routinesunsafe-legacy-renegotiation-disabled/76012131#76012131
# The following line fixes the issue, so that project images can once again
# use remotes::install_gitlab to install city of edmonton packages.
# We have put it in the base image in order to make things simple, however we 
# should keep an eye out for a fix to this issue, and remove this line when that 
# happens.
# Note, this could be called in the same RUN command as install_gitlab, and then
# sed -i "$d" /etc/ssl/openssl.cnf  could be used to "undo" it, but we decided 
# against this approach because this would mean the fix is distributed across
# many projects.
# Note that this fix actually gives us the same security setup as we had on 
# earlier (0.3.*) images.
RUN echo "Options = UnsafeLegacyRenegotiation" >> /etc/ssl/openssl.cnf 

RUN mkdir /shinyapp
WORKDIR /shinyapp/
ADD ./ ./

RUN useradd shiny -u 5000 -m -b /home
RUN chown -R shiny:shiny /shinyapp
USER shiny

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/shinyapp', host = '0.0.0.0', port = 3838)"]
