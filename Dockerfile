from ptagliolato/r-spatial-base:1.0.0

COPY ui.R root/TELLme-vlab-OSMDataFromRelatedConcept
COPY server.R root/TELLme-vlab-OSMDataFromRelatedConcept
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/TELLme-vlab-OSMDataFromRelatedConcept')"]

