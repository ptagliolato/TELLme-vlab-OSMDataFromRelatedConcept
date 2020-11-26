FROM ptagliolato/r-spatial-base:1.0.2

MAINTAINER ptagliolato

#RUN mkdir /root/TELLme-vlab-OSMDataFromRelatedConcept
COPY ui.R root/TELLme-vlab-OSMDataFromRelatedConcept/ui.R
COPY server.R root/TELLme-vlab-OSMDataFromRelatedConcept/server.R
COPY global.R root/TELLme-vlab-OSMDataFromRelatedConcept/global.R
COPY utils.R root/TELLme-vlab-OSMDataFromRelatedConcept/utils.R
COPY tellmeFunctions.R root/TELLme-vlab-OSMDataFromRelatedConcept/tellmeFunctions.R

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/TELLme-vlab-OSMDataFromRelatedConcept')"]

