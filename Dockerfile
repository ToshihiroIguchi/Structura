# ベースイメージに Shiny 環境を持つ rocker/shiny を指定
FROM rocker/shiny:latest

# システムライブラリのインストール（devtools やビルドに必要な依存）
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      git && \
    rm -rf /var/lib/apt/lists/*

# R パッケージのインストール
# まず devtools 本体を入れる
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"

# GitHub から semDiagram と readflex を開発版インストール
RUN R -e "devtools::install_github('ToshihiroIguchi/semDiagram', \
      dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"
RUN R -e "devtools::install_github('ToshihiroIguchi/readflex', \
      dependencies=TRUE, upgrade='never', build=FALSE, build_vignettes=FALSE)"

# その他 CRAN パッケージのインストール
RUN R -e "install.packages(c( \
      'shiny', 'shinyjs', 'DT', 'rhandsontable', \
      'lavaan', 'DiagrammeR', 'ggplot2', 'reshape2', \
      'markdown', 'scales' \
    ), repos='https://cloud.r-project.org/')"

# アプリケーションコードをコンテナ内にコピー
# （ローカルで `docker build` を実行する際のコンテキストはリポジトリのルート）
COPY . /srv/shiny-server/Structura

# 権限設定：shiny ユーザーがアクセスできるように
RUN chown -R shiny:shiny /srv/shiny-server

# Shiny Server のデフォルトポートを expose
EXPOSE 3838

# デフォルトのコマンドで Shiny Server を起動
CMD ["/usr/bin/shiny-server", "/srv/shiny-server/Structura"]
