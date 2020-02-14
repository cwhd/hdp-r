FROM rocker/r-ver:3.5.1

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    wget \
    libssl-dev \
    libsasl2-dev \
    libxml2-dev


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'mongolite','data.tree','rjson'), repos='$MRAN',   dependencies=TRUE)" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

RUN R -e "install.packages(c('devtools','DT'), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('cwhd/hdm-r')"

EXPOSE 80
EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/

RUN chmod +x /usr/bin/shiny-server.sh
CMD ["/usr/bin/shiny-server.sh"]
