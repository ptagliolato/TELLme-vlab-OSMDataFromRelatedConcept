from ptagliolato/r-spatial-base:1.0.0
#RUN mkdir /root/TELLme-vlab-OSMDataFromRelatedConcept
COPY ui.R root/TELLme-vlab-OSMDataFromRelatedConcept/ui.R
COPY server.R root/TELLme-vlab-OSMDataFromRelatedConcept/server.R
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/TELLme-vlab-OSMDataFromRelatedConcept')"]

