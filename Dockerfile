FROM bioinstaller/opencpu:latest

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@life2cloud.com

Run apt update && apt install -y \
    r-cran-ape \
    r-cran-digest \
    r-cran-bitops \
    r-cran-futile.logger \
    r-cran-foreach \
    r-cran-formatr \
    r-cran-gdata \
    r-cran-gplots \
    r-cran-httpuv \
    r-cran-igraph \
    r-cran-jsonlite \
    r-cran-plyr \
    r-cran-rcppeigen \
    r-cran-rcpparmadillo \
    r-cran-rcurl \
    r-cran-snow \
    r-cran-stringi \
    r-cran-xml \
    r-cran-yaml \
    r-cran-zoo \
    && Rscript -e "devtools::install_github('Miachol/configr', ref = 'develop')" \
    && Rscript -e "devtools::install_github('JhuangLab/ngstk', ref = 'develop')" \
    && Rscript -e "devtools::install_github('JhuangLab/BioInstaller', ref = 'develop')" \
    && Rscript -e "source('https://bioconductor.org/biocLite.R');biocLite(ask = FALSE)" \
    && Rscript -e "devtools::install_url('http://www.bioconductor.org/packages/release/bioc/src/contrib/CEMiTool_1.4.0.tar.gz')" \ 
    && Rscript -e "devtools::install('.')" \ 
    && Rscript -e "annovarR::check_shiny_dep(TRUE)" \
    && ln -s /usr/local/lib/R/site-library/annovarR/extdata/tools/shiny/R /srv/shiny-server/annovarR \
    && mkdir -p /home/opencpu/.annovarR/tools/ \
    && mkdir -p /home/opencpu/.annovarR/db/ \
    && chown -R opencpu /home/opencpu/.annovarR \
    && echo 'LC_ALL="en_US.UTF-8"\nLANG="en_US.UTF-8"' >> /etc/R/Renviron \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
    && rm -rf /var/lib/apt/lists/* \
    && rm /etc/apache2/sites-enabled/rstudio.conf

ADD ./inst/docker/shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./inst/docker/shiny-apache.conf /etc/apache2/sites-enabled/000-default.conf

ADD http://bioinfo.rjh.com.cn/download/bioinstaller/vcfanno/v0.2.9/vcfanno_linux64 /home/opencpu/.annovarR/tools/vcfanno
ADD http://bioinfo.rjh.com.cn/download/bioinstaller/annovar/annovar.latest.tar.gz /home/opencpu/.annovarR/tools/annovar.latest.tar.gz

RUN cd /home/opencpu/.annovarR/tools/ && tar -xzvf annovar.latest.tar.gz \
    && rm annovar.latest.tar.gz && chown -R opencpu /home/opencpu/.annovarR \
    && chmod a+x /home/opencpu/.annovarR/tools/vcfanno

RUN ln -s /home/opencpu/.annovarR/tools/annovar/humandb/* /home/opencpu/.annovarR/db

CMD service cron start && runuser -l opencpu -c 'Rscript -e "annovarR::set_shiny_workers(3)"' && runuser -l opencpu -c 'sh /usr/bin/start_shiny_server' && /usr/lib/rstudio-server/bin/rserver && apachectl -DFOREGROUND
