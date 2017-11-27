FROM bioinstaller/bioinstaller:develop

## This handle reaches Jianfeng
MAINTAINER "Jianfeng Li" lee_jianfeng@life2cloud.com

Run Rscript -e "devtools::install_github('JhuangLab/annovarR', ref = 'develop')"

CMD ["R"]
