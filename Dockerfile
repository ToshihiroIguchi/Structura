# ベースイメージ
FROM rocker/shiny:latest

# 必要なシステム依存ライブラリのインストール
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      git \
      libgit2-dev \
      zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# remotes パッケージをインストール（devtools の代替）
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# GitHub 版パッケージを remotes でインストール
RUN R -e "remotes::install_github('ToshihiroIguchi/semDiagram', \
        dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)" && \
    R -e "remotes::install_github('ToshihiroIguchi/readflex', \
        dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"

# CRAN パッケージ群をまとめてインストール
RUN R -e "install.packages(c( \
      'shiny','shinyjs','DT','rhandsontable', \
      'lavaan','DiagrammeR','ggplot2','reshape2', \
      'markdown','scales' \
    ), repos='https://cloud.r-project.org/')"

# アプリケーションコードを配置
COPY . /srv/shiny-server/Structura
RUN chown -R shiny:shiny /srv/shiny-server/Structura

# ポートを公開
EXPOSE 3838

# Shiny Server 起動
CMD ["/usr/bin/shiny-server"]
