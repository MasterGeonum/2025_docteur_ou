FROM rocker/r2u:jammy

LABEL org.opencontainers.image.source="https://github.com/mastergeonum/2025_docteur_ou"

RUN Rscript -e 'install.packages("bslib")'
RUN Rscript -e 'install.packages("bslib")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("htmltools")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("RColorBrewer")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("shinywidgets")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("units")'

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('./main', port=3838, host='0.0.0.0')"]