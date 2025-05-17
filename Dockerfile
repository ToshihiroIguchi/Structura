# Base image: Use the official rocker/shiny image with Shiny Server pre-installed
FROM rocker/shiny:latest

# Install necessary system libraries and tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        git \
        iproute2 && \
    rm -rf /var/lib/apt/lists/*

# Install the 'remotes' package to enable installation of GitHub packages
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install 'semDiagram' and 'readflex' packages from GitHub
RUN R -e "remotes::install_github('ToshihiroIguchi/semDiagram', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)" && \
    R -e "remotes::install_github('ToshihiroIguchi/readflex', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"

# Install required CRAN packages for the Shiny application
RUN R -e "install.packages(c('shiny','shinyjs','DT','rhandsontable','lavaan','DiagrammeR','ggplot2','reshape2','markdown','scales'), repos='https://cloud.r-project.org/')"

# Remove default Shiny Server sample applications and static pages
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/welcome.html  \
           /srv/shiny-server/index.html    \
           /srv/shiny-server/*.html        \
           /srv/shiny-server/0*

# Copy the Structura application into the Shiny Server directory
COPY . /srv/shiny-server/Structura

# Set ownership of the application directory to the 'shiny' user
RUN chown -R shiny:shiny /srv/shiny-server/Structura

# Expose the default Shiny Server port
EXPOSE 3838

# Start the Shiny Server
CMD ["/usr/bin/shiny-server"]
