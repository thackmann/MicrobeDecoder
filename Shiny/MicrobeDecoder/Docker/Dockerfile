#Get base Docker image
FROM rocker/shiny:latest

# Install system dependencies for Plotly and other packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libboost-all-dev \
    pandoc \
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by the Shiny app
RUN R -e "install.packages(c( \
  'ape', \
  'BiocManager', \
  'bslib', \
  'caret', \
  'colorspace', \
  'digest', \
  'dplyr', \
  'DT', \
  'fbar', \
  'fs', \
  'htmltools', \
  'igraph', \
  'import', \
  'jqbr', \
  'jsonlite', \
  'magrittr', \
  'plotly', \
  'purrr', \
  'randomForest', \
  'reactlog', \
  'readr', \
  'readxl', \
  'remotes', \
  'rlang', \
  'rsconnect', \
  'shiny', \
  'shinycssloaders', \
  'shinyjs', \
  'shinyWidgets', \
  'stringr', \
  'tidyr', \
  'tidyselect', \
  'uuid' \
))"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly=TRUE)) install.packages('BiocManager'); BiocManager::install('ggtree', ask=FALSE, update=FALSE)"

# Install GitHub packages
RUN R -e "remotes::install_github('thackmann/FileLocator', subdir='FileLocator')"

# Modify configuration file for Shiny Server
## Set infinite idle timeout
RUN if grep -q "app_idle_timeout" /etc/shiny-server/shiny-server.conf; then \
      sed -i 's/app_idle_timeout.*/app_idle_timeout 0;/' /etc/shiny-server/shiny-server.conf; \
    else \
      sed -i '/server {/a \  app_idle_timeout 0;' /etc/shiny-server/shiny-server.conf; \
    fi

# Copy app files
COPY . /srv/shiny-server/

#Select port
EXPOSE 3838

#Allow permission
RUN chown -R shiny:shiny /srv/shiny-server

#Copy further configuration files into the Docker image
COPY ../Docker/shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

#Run app
CMD ["/usr/bin/shiny-server.sh"]