FROM rocker/verse:4.4.1
WORKDIR /home/rstudio
RUN apt-get update -y && apt-get install -y rsync
RUN tlmgr update --self && tlmgr install collection-latexrecommended
