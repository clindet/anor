FROM bioinstaller/bioinstaller:develop

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@life2cloud.com

Run apt-get update \
    && apt-get install -t unstable -y r-cran-rmysql \
    && Rscript -e "devtools::install_github('JhuangLab/annovarR', ref = 'develop')"

CMD ["R"]
