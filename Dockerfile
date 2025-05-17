# Use the official rocker/shiny image with Shiny Server pre-installed
FROM rocker/shiny:latest

# Install system dependencies for building R packages, Git, and network utilities
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      git \
      iproute2 && \
    rm -rf /var/lib/apt/lists/*

# Install remotes package to enable installing GitHub packages
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install development versions of semDiagram and readflex from GitHub
RUN R -e "remotes::install_github('ToshihiroIguchi/semDiagram', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)" && \
    R -e "remotes::install_github('ToshihiroIguchi/readflex', dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"

# Install CRAN packages required by the Shiny application
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'rhandsontable', 'lavaan', 'DiagrammeR', 'ggplot2', 'reshape2', 'markdown', 'scales'), repos='https://cloud.r-project.org/')"

# Remove default Shiny Server sample apps and static pages
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/welcome.html \
           /srv/shiny-server/index.html \
           /srv/shiny-server/*.html \
           /srv/shiny-server/0*

# Copy the Shiny application code to the Shiny Server directory
COPY . /srv/shiny-server/Structura

# Ensure proper ownership so Shiny Server can serve the files
RUN chown -R shiny:shiny /srv/shiny-server/Structura

# Create an entrypoint script that prints LAN URL and starts Shiny Server
RUN bash -c 'cat <<"EOS" > /usr/local/bin/entrypoint.sh
#!/bin/bash
# Detect host-side gateway IP reachable from this container
HOST_IP=$(ip route get 8.8.8.8 | awk "NR==1 {print \$3}")
# Fallback when ip route fails (e.g., host mode)
if [ -z "${HOST_IP}" ]; then HOST_IP=$(hostname -I | awk "{print \$1}"); fi

echo "Shiny App available locally : http://localhost:3838/Structura"
echo "Shiny App available on LAN  : http://${HOST_IP}:3838/Structura"

exec /usr/bin/shiny-server
EOS' && chmod +x /usr/local/bin/entrypoint.sh

# Expose Shiny Server port
EXPOSE 3838

# Launch using the entrypoint script
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
