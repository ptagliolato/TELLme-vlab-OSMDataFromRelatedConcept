source("utils.R")
source("tellmeFunctions.R")

# you can create your own use _private.settings.R 
# and create there the required environment variable 
# (i.e. GETIT_ADMIN_USER, GETIT_ADMIN_PASSWORD, GEOSERVER_ADMIN_USER, GEOSERVER_ADMIN_PASSWORD)
# with Sys.setenv function
tryCatch(source("_private.settings.R"), error=function(e){warning("not found _private.settings.R file")})
# in Docker you should pass these variables as env parameters when running the container

ENABLE_UPLOAD<-as.logical(Sys.getenv("ENABLE_UPLOAD", unset=FALSE))

getit_url<-"http://tellmehub.get-it.it"
getit_superuser<-Sys.getenv("GETIT_ADMIN_USER", unset="admin") # admin user in the get-it: it is needed to invoke the updatelayers rest API
getit_superuser_password<-Sys.getenv("GETIT_ADMIN_PASSWORD")  # it is the admin password
geoserver_url<-"http://tellmehub.get-it.it/geoserver"
workspacename<-"geonode" # it should be this one for the TELLme project
datastorename<-"geonode_data" # same as previous
datastore_postgis_name<-"geonode"
geoserver_user<-Sys.getenv("GEOSERVER_ADMIN_USER", unset="admin") #geoserver admin user
geoserver_password<-Sys.getenv("GEOSERVER_ADMIN_PASSWORD") #geoserver admin password

