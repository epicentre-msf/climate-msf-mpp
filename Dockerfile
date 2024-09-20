FROM epicentremsf/epishiny:ubuntu

LABEL maintainer="Paul Campbell paul.campbell@epicentre.msf.org"

RUN install2.r --error --skipinstalled sfnetworks
RUN R -e 'remotes::install_github("rstudio/bslib")'
