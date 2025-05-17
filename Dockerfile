# Use the official rocker/shiny image with Shiny Server pre-installed
FROM rocker/shiny:latest

# Install system dependencies for building R packages and Git
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      git && \
    rm -rf /var/lib/apt/lists/*

# Install remotes package to enable installing GitHub packages
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install development versions of semDiagram and readflex from GitHub
RUN R -e "remotes::install_github('ToshihiroIguchi/semDiagram', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)" && \
    R -e "remotes::install_github('ToshihiroIguchi/readflex', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"

# Install CRAN packages required by the Shiny application
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'rhandsontable', 'lavaan', 'DiagrammeR', 'ggplot2', 'reshape2', 'markdown', 'scales'), repos='https://cloud.r-project.org/')"

# Remove default Shiny Server sample apps and static welcome/index pages to prevent sample app display
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/welcome.html \
           /srv/shiny-server/index.html \
           /srv/shiny-server/*.html

# Copy the Shiny application code to the Shiny Server directory
COPY . /srv/shiny-server/Structura

# Ensure proper ownership so Shiny Server can serve the files
RUN chown -R shiny:shiny /srv/shiny-server/Structura

# Expose the Shiny Server default port
EXPOSE 3838

# Run Shiny Server using the default configuration
CMD ["/usr/bin/shiny-server"]
