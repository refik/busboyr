FROM 575677476286.dkr.ecr.us-east-1.amazonaws.com/busboy-base:latest

# Adding configurations
ADD shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD Rprofile.site /etc/R/Rprofile.site

# Pulling latest code and installing busboyr while ignoring dependencies.
RUN cd /busboyr && git pull && R -e "devtools::install('/busboyr', upgrade_dependencies = FALSE)"

# A small utility program for watching multiple log files
ADD https://raw.githubusercontent.com/madivad/retail/master/retail.c /tmp/retail.c
RUN gcc /tmp/retail.c -o /usr/bin/retail && chmod +x /usr/bin/retail

COPY script/ /usr/bin/
RUN chmod +x /usr/bin/busboy-*

# Open port
EXPOSE 3838