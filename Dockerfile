FROM opencpu/rstudio

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@life2cloud.com

ADD https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb /tmp/shiny-server-1.5.7.907-amd64.deb

Run apt-get update \
    && apt-get install -y libcairo2-dev \
    && apt-get install -y librsvg2-dev \
    && apt-get install -y libmariadb-client-lgpl-dev \
    && apt-get install -y libxt-dev \
    && apt-get install -y gdebi-core \
    && apt-get install -y sqlite3 \
    && echo "options('repos' = c(CRAN='https://mirrors.tuna.tsinghua.edu.cn/CRAN/'))" >> ~/.Rprofile \
    && echo "options(BioC_mirror='https://mirrors.tuna.tsinghua.edu.cn/bioconductor')" >> ~/.Rprofile \
    && Rscript -e "devtools::install_github('Miachol/configr', ref = 'develop')" \
    && Rscript -e "devtools::install_github('JhuangLab/ngstk', ref = 'develop')" \
    && Rscript -e "devtools::install_github('JhuangLab/BioInstaller', ref = 'develop')" \
    && Rscript -e "source('https://bioconductor.org/biocLite.R');biocLite(ask = FALSE)" \
    && Rscript -e "devtools::install_github('JhuangLab/annovarR')" \
    && Rscript -e "annovarR::check_shiny_dep(TRUE)" \
    && gdebi -n /tmp/shiny-server-1.5.7.907-amd64.deb \
    && chown -R opencpu /var/log/shiny-server \
    && chown -R opencpu /opt/shiny-server \
    && ln -s /usr/local/lib/R/site-library/annovarR/extdata/tools/shiny/R /srv/shiny-server/annovarR \
    && echo "shiny-server &" >> /tmp/start_shiny_server \
    && a2enmod rewrite\
    && a2enmod proxy_http \
    && a2enmod proxy_wstunnel \
    && rm /tmp/shiny-server-1.5.7.907-amd64.deb \
    && apt autoclean \
    && apt clean

ADD ./inst/docker/shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./inst/docker/shiny-apache.conf /etc/apache2/sites-enabled/000-default.conf

CMD service cron start && /usr/lib/rstudio-server/bin/rserver && sh /tmp/start_shiny_server && apachectl -DFOREGROUND
