FROM bioinstaller/ngsjs:latest

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@openbiox.org

ADD . /tmp/annovarR

Run runuser -s /bin/bash -l opencpu -c "Rscript -e 'pacman::p_load(maftools, clusterProfiler, CEMiTool, org.Hs.eg.db, DT)'" \
    && runuser -s /bin/bash -l opencpu -c "Rscript -e 'devtools::install(\"/tmp/annovarR\", dependences = T)'" \
    && rm -rf /tmp/annovarR \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
