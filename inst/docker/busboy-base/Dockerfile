FROM ubuntu:16.04

# Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive

# Install basic stuff
RUN apt update && apt upgrade -y && apt install -y \
    wget \
    git \
    vim \
    libpq-dev \
    libmariadb-client-lgpl-dev \
    software-properties-common \
    gdebi-core \
    libcurl4-openssl-dev \
    libxml2-dev

# Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $(lsb_release -cs)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Install R
RUN apt install -y r-base

# Install shiny and devtools
RUN R -e "install.packages(c('shiny', 'devtools'), repos='https://cran.rstudio.com/')"

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Pull busboyr repository
RUN git clone https://c7ffe49b2ba265e195f69afb8a965de1c910a54f:x-oauth-basic@github.com/refik/busboyr.git /busboyr
RUN R -e "devtools::install('/busboyr')"