FROM b2bwebid/shiny-server:jessie
MAINTAINER B2B.Web.ID Market Analist
ADD install.R /root
RUN Rscript /root/install.R && rm /root/install.R
ADD shiny-server /srv/shiny-server
