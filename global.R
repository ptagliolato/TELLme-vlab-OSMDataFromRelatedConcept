source("utils.R")
source("tellmeFunctions.R")

# maybe these variable could be inherited from some external settings (the .env dockerfile)?
getit_url<-"http://tellmehub.get-it.it"
getit_superuser<-"admin" # admin user in the get-it: it is needed to invoke the updatelayers rest API
getit_password<-""  # it is the admin password
geoserver_url<-"http://tellmehub.get-it.it/geoserver"
workspacename<-"geonode" # it should be this one for the TELLme project
datastorename<-"vlab" # same as previous
datastore_postgis_name<-"geonode"
geoserver_user<-"admin" #geoserver admin user
geoserver_password<-"" #geoserver admin password

shinyuser2getituser<-function(user){
  #the function must associate each user in the application.yml file to a user in the TELLme-hub
  
  #if(user=="")rxeturn("")
  switch(user,
         MEXICO="MEXICO_CITY"
         #,...
         )
}
