FROM bioinstaller/ngsjs:latest

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@openbiox.org

ADD . /tmp/annovarR

Run runuser -s /bin/bash -l opencpu -c "Rscript -e 'pacman::p_load(maftools, clusterProfiler, CEMiTool)'" \
    && runuser -s /bin/bash -l opencpu -c "R CMD INSTALL /tmp/annovarR"
    && rm -rf /tmp/annovarR \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
