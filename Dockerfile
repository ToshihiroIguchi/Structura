# ---------- Base image ----------
FROM rocker/shiny:latest

# ---------- System libraries ----------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        git \
        iproute2 && \
    rm -rf /var/lib/apt/lists/*

# ---------- R packages ----------
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('ToshihiroIguchi/semDiagram', dependencies = TRUE, upgrade = 'never', build = FALSE, build_vignettes = FALSE)"
RUN R -e "remotes::install_github('ToshihiroIguchi/readflex',   dependencies = TRUE, upgrade = 'never', build = FALSE, build_vignettes = FALSE)"
RUN R -e "install.packages(c('shiny','shinyjs','DT','rhandsontable','lavaan','DiagrammeR','ggplot2','reshape2','markdown','scales'), repos = 'https://cloud.r-project.org/')"

# ---------- Clean up sample apps ----------
RUN rm -rf /srv/shiny-server/sample-apps \
           /srv/shiny-server/welcome.html  \
           /srv/shiny-server/index.html    \
           /srv/shiny-server/*.html        \
           /srv/shiny-server/0*

# ---------- Copy application ----------
COPY . /srv/shiny-server/Structura
RUN chown -R shiny:shiny /srv/shiny-server/Structura

# ---------- Entrypoint script ----------
RUN bash -c 'cat << "EOF" > /usr/local/bin/entrypoint.sh
#!/usr/bin/env bash
# Detect host-side IP (bridge gateway) — falls back to first non-loopback IP
HOST_IP=$(ip route | awk "/default/ {print \$3; exit}")
[ -z "\$HOST_IP" ] && HOST_IP=$(hostname -I | awk "{print \$1}")

echo "Shiny App available locally : http://localhost:3838/Structura"
echo "Shiny App available on LAN  : http://\$HOST_IP:3838/Structura"

exec /usr/bin/shiny-server
EOF'
RUN chmod +x /usr/local/bin/entrypoint.sh

# ---------- Expose port & start ----------
EXPOSE 3838
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
