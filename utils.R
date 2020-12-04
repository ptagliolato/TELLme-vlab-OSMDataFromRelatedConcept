library(geosapi)
library(httr)

#geoserver_url must comprise protocol and be without trailing slash (e.g. "http://tellmehub.get-it.it/geoserver")
#
# upload a layer in geoserver, setting one keyword for that layer.
# 

geoserver<-function(geoserver_url,geoserver_user,geoserver_password){
  geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
}

getit_geoserver_delete_layer<-function(geoserver_url, geoserver_user, geoserver_password, layername, workspacename="geonode",datastorename="vlab"){
  geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
  geoserver$deleteLayer(lyr = layername)
  geoserver$unpublishLayer(ws = workspacename,ds = datastorename,lyr = layername)
}

# NOTE: the zip file must be named <layername>.zip
getit_geoserver_upload_layer<-function(geoserver_url,
                                       workspacename="geonode",
                                       datastorename="vlab",
                                       geoserver_user="admin",
                                       geoserver_password,
                                       layername,
                                       keyword=NULL #Sets the keyword only if specified
                                       #,style_name,
                                       #getituser,
                                       #getitpassword,
                                       #layer_owner_getit_username
){
  #geoserver_url<-paste0(getiturl,"/geoserver")#http://tellmehub.get-it.it/geoserver"
  zipname<-paste0(layername,".zip") #NOTE: zip name must coincide with the name of internal shapefile!!!!
  #shapename<-paste0(layername,".shp")
  
  url<-paste0(geoserver_url,"/rest/workspaces/",workspacename,"/datastores/",datastorename,"/file.shp")
  
  # come dovrebbe essere caricato con curl
  curlsentence<-paste0("curl -v -u ", geoserver_user, ":", geoserver_password, " -XPUT -H \"Content-type: application/zip\" --data-binary @", zipname, " ", url)
  cat(curlsentence)
  
  # procedo con httr
  file<-httr::upload_file(paste0(getwd(),"/",zipname))
  
  contentType<-"application/zip"
  token <- openssl::base64_encode(charToRaw(paste(geoserver_user, geoserver_password, sep=":")))
  httr::PUT(url = url,
            httr::add_headers(
              #"User-Agent" = GSUtils$getUserAgent(),
              "Authorization" = paste("Basic", token ),
              "Content-type" = contentType
            ),    
            body = file
  )
  
  if(!is.null(keyword)){
    geoserver_layer_set_keyword(geoserver_url, geoserver_user, geoserver_password,layername,keyword)
  }
  # 
  # # connessione con geosapi
  # geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
  # 
  # # impostazione keyword del layer (feature type, tecnicamente) appena caricato
  # geoserver$getFeatureTypeNames(workspacename,datastorename)
  # 
  # #tellme_keyword<-"docks"
  # featureType<-geoserver$getFeatureType(workspacename,datastorename,layername)
  # featureType$setKeywords(list(keyword))
  # geoserver$updateFeatureType(workspacename, datastorename, featureType)
}

geoserver_layer_set_keyword<-function(geoserver_url, geoserver_user="admin", geoserver_password,
                                      workspacename="geonode",
                                      datastorename="geonode_data",
                                      layername,
                                      keyword){
  
  # connessione con geosapi
  geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
  
  # impostazione keyword del layer (feature type, tecnicamente) appena caricato
  geoserver$getFeatureTypeNames(workspacename,datastorename)
  
  #tellme_keyword<-"docks"
  featureType<-geoserver$getFeatureType(workspacename,datastorename,layername)
  featureType$setKeywords(list(keyword))
  geoserver$updateFeatureType(workspacename, datastorename, featureType)
}

geoserver_layer_add_style<-function(geoserver_url, geoserver_user="admin", geoserver_password,layername,stylename){
  geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
  
  layer<-geoserver$getLayer(layername)
  
  layer$addStyle(stylename)
  layer$styles
  layer$setDefaultStyle(stylename)
  
  geoserver$updateLayer(layer = layer)
}

# geoserver_getSLD_body<-function(geoserver_url, geoserver_user="admin", geoserver_password,stylename){
#   # hack version number (workaround to unlock getSLD body of geosapi)
#   geoserver$version$version=2.2
#   geoserver$version$value$minor<-2
#   geoserver$version$value$major<-2
#   sldbody<-geoserver$getSLDBody(stylename)
#   return(sldbody)
# }


# set the style with the given name as default for the layer (register the previous default style in the layer style list)
geoserver_layer_set_default_style<-function(geoserver_url, geoserver_user="admin", geoserver_password,layername,stylename){
  geoserver<-GSManager$new(geoserver_url, geoserver_user, geoserver_password, "DEBUG")
  
  # if stylename is not present in geoserver, abort operation and return FALSE
  if(!stylename %in% geoserver$getStyleNames()){
    return(FALSE)
  }
  
  layer<-geoserver$getLayer(layername)
  currentDefaultStyleName<-layer$defaultStyle$name
  
  layer$addStyle(currentDefaultStyleName)
  #geoserver$updateLayer(layer=layer)
  
  # layer$styles
  if(stylename %in% sapply(layer$styles,function(s){paste0(s$name)})){
    layer$setDefaultStyle(stylename)
  }
  
  geoserver$updateLayer(layer = layer)
}


getit_authenticate_handle<-function(getit_url, getit_user, getit_password){

  # bisogna andare sulla pagina di login e recuperare csrfmiddlewaretoken dal modulo che si presenta
  # poi bisogna usarlo per effettuare l'autenticazione via una POST con Content-Type: application/x-www-form-urlencoded
  # (vedi curl qui sotto)
  # poi bisogna usare i cookie di sessione (httr sembrerebbe saperlo fare da solo...)
  # cfr altrimenti come fare con rcurl
  #   https://stackoverflow.com/questions/15000815/post-request-using-cookies-with-curl-rcurl-and-httr
  #
  # curl 'http://tellmehub.get-it.it/account/login/?next=/'
  # --compressed
  # -H 'Content-Type: application/x-www-form-urlencoded'
  # -H 'Origin: http://tellmehub.get-it.it'
  # -H 'Referer: http://tellmehub.get-it.it/'
  # -H 'Cookie: csrftoken=D1xYaa7nClhLCgp7SPEoBQcnXpwQDVlh; _ga=GA1.2.952619385.1568619305'
  # -H 'Upgrade-Insecure-Requests: 1'
  # --data 'csrfmiddlewaretoken=D1xYaa7nClhLCgp7SPEoBQcnXpwQDVlh&username=admin&password=admin'

  urlauth<-paste0(getit_url,"/account/login/")
  res<-httr::GET(url=urlauth) # questo dovrebbe servire a settare i cookie necessari alla POST successiva

  # obtain csfrtoken
  s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
  s_split<-strsplit(s,";")
  s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
  csfrtoken<-sub("csrftoken=","",s_kv)

  reslogin<-httr::POST(url=urlauth,
                       body=list(csrfmiddlewaretoken=csfrtoken,
                                 username=getit_user,
                                 password=getit_password)
  )

  return(reslogin$handle)
}

# refresh get-it layers from geoserver
getit_updatelayers<-function(getit_url, getit_superuser, getit_password, workspacename, datastorename, layername, ownername=NULL){
  # # compose request for the API updatelayers
  # ownerparam<-""
  # if(!is.null(ownername)){
  #   ownerparam<-paste0("&owner=",ownername)
  # }
  # #  r<-paste0(getit_url,"/gs/updatelayers/?","workspace=",workspacename,"&store=",storename,"&filter=",layername, ownerparam)
  #getit_token<-"pyxW5djJ7XsjeFUXduAsGpR4xMGUwpeBGQRqTeT3" #TASTYPIE_APIKEY
  url<-paste0(getit_url,"/gs/updatelayers/")
  
  query= list(workspace=workspacename,store=datastorename,filter=layername)
  if(!is.null(ownername)){
    query$owner=ownername
  }
  
  #handle=getit_authenticate_handle(getit_url, getit_superuser, getit_password)
  {
    urlauth<-paste0(getit_url,"/account/login/")
    res<-httr::GET(url=urlauth) # questo dovrebbe servire a settare i cookie necessari alla POST successiva
    
    # obtain csfrtoken
    s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
    s_split<-strsplit(s,";")
    s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
    csfrtoken<-sub("csrftoken=","",s_kv)
    
    reslogin<-httr::POST(url=urlauth,
                         body=list(csrfmiddlewaretoken=csfrtoken,
                                   username=getit_superuser,
                                   password=getit_password)
    )
  }
  
  
  res<-httr::GET(url = url,
                 query=query
  )
  return(res)
  
  
}


# layertitle: field title of both get-it and geoserver. It will be keeped as passed (e.g.uppercase)
# on the contrary, the layer name is for example lowercased version of the Title
getit_uploadLayer<-function(getit_url, getit_user,getit_userpassword, zipname_and_path, layertitle=NULL){
  zipname<-basename(zipname_and_path)
  #layername<-layertitle
  geoserver_layername=sub(".zip","",zipname,fixed = T)
  if(is.null(layertitle)){
    layertitle=geoserver_layername
    #TODO: verificare nome layer in geoserver. Potrebbe 1. alterare alcuni caratteri; 2. non coincidere con layertitle passato al get-it
    # in questo caso bisogna agire di conseguenza sulla parte di codice che assegna la keyword e che poi esegue updatelayers
  }
  
  ### autenticazione
  {
    urlauth<-paste0(getit_url,"/account/login/")
    res<-httr::GET(url=urlauth) # questo dovrebbe servire a settare i cookie necessari alla POST successiva
    
    # obtain csfrtoken
    s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
    s_split<-strsplit(s,";")
    s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
    csfrtoken<-sub("csrftoken=","",s_kv)
    
    reslogin<-httr::POST(url=urlauth,
                         body=list(csrfmiddlewaretoken=csfrtoken,
                                   username=getit_user,
                                   password=getit_userpassword)
    )
  }
  
  #CSRFToken=csfrtoken #no: questo è quello vecchio
  
  #csrftoken<-
  
  ccc<-cookies(reslogin)
  loggedin_token<-ccc$value[ccc$name=="csrftoken"]
  loggedin_session<-ccc$value[ccc$name=="sessionid"]
  #prevcookies<-httr::cookies(reslogin)
  #httr::cookies(response)
  #httr::set_cookies(prevcookies)
  #reset_config()
  response<-httr::POST(paste0(getit_url,"/layers/upload"),
             config = httr::add_headers(`X-CSRFToken`=loggedin_token, Accept="*/*", 
                                        Origin=getit_url, 
                                        Connection="keep-alive", 
                                        Referer=paste0(getit_url,"/layers/upload")
                                        ),
             body=list(base_file=upload_file(zipname_and_path),
                       permissions='{"users":{"AnonymousUser":["view_resourcebase","download_resourcebase"]},"groups":{}}',
                       zip_file=upload_file(zipname_and_path),
                       charset="UTF-8",
                       abstract="Obtained by TELLme-Vlab OSMDataFromRelatedConcept. Data: (c) Open Street Map contributors. License: ODbL",
                       layer_title=layertitle)
  )
  # extract geoserver_layername from response if possible
  getit_layerurl<-content(response)$url
  getit_layername<-sub("/layers/","",getit_layerurl)
  geoserver_layertitle<-layertitle # it is unchanged with respect to the layertitle passed through the POST request
  geoserver_layername<-getit_layername
  # TODO: check return values for names of the layers in geoserver and in get-it: are they correct? are they the same?
  return(list(status=status_code(response),getit_layername=getit_layername, geoserver_layername=geoserver_layername, getit_layerurl=getit_layerurl, response=response))
}

getit_auth.GET<-function(getit_url,getit_user,getit_password,s_url){
  #h<-getit_authenticate_handle(getit_url,getit_user,getit_password)
  {
    urlauth<-paste0(getit_url,"/account/login/")
    res<-httr::GET(url=urlauth) # questo dovrebbe servire a settare i cookie necessari alla POST successiva
    
    # obtain csfrtoken
    s<-res$headers$`set-cookie` # e.g. "csrftoken=saknwlpo8G64UKJDR7UGxNbue184D9if; expires=Mon, 08-Feb-2021 15:12:47 GMT; Max-Age=31449600; Path=/"
    s_split<-strsplit(s,";")
    s_kv<-grep("csrftoken",fixed=TRUE,s_split[[1]],value=TRUE)
    csfrtoken<-sub("csrftoken=","",s_kv)
    
    reslogin<-httr::POST(url=urlauth,
                         body=list(csrfmiddlewaretoken=csfrtoken,
                                   username=getit_user,
                                   password=getit_password)
    )
  }
  
  res<-httr::GET(s_url)
  return(res)
}

#  set_conceptid via API 
tellmehub_layername_set_conceptid<-function(getit_url,getit_user,getit_password,layername,concept_id, add=FALSE){
  s=sprintf(paste0(getit_url,"/tellme/layers/%s/set_conceptid/%d"), layername,concept_id)
  if(add) s=paste0(s,"?add=TRUE")
  
  res<-getit_auth.GET(getit_url,getit_user,getit_password,s)
  return(res)
}

tellmehub_api_clean_missing_style_title<-function(getit_url,getit_user,getit_password){
  s=sprintf(paste0(getit_url,"/tellme/cleanMissingStyleTitle"))
  res<-getit_auth.GET(getit_url,getit_user,getit_password,s)
  # res$content
  return(res)
}

tellmehub_delete_non_tellme_hierarchicalKeywords<-function(getit_url,getit_user,getit_password){
  s=sprintf(paste0(getit_url,"/tellme/delete_non_tellme_hierarchicalKeywords"))
  res<-getit_auth.GET(getit_url,getit_user,getit_password,s)
  # res$content
  return(res)
}

# # TODO: add parameters and complete the method. CHECK THIS METHOD AFTER THE PREVIOUS ONE
# getit_uploadLayer_and_setKeyword(getit_url, getit_user, getit_userpassword, zipname, layertitle=NULL, keyword, geoserver_password){
#   getit_uploadLayer(getit_url, getit_user, getit_userpassword, geoserver_layername, layertitle)
#   # TODO: insert here code for managing keywords (e.g. new API in get-it or passing through geoserver and then making an updatelayers request 
#   # throgh admin user)
#   # TODO: refactor the following part adding a separate method that invokes
#   if(!is.null(keyword)){
#     geoserver_url=paste0(getit_url,"/geoserver")
#     geoserver_layer_set_keyword(geoserver_url = geoserver_url, geoserver_user="admin" ,geoserver_password = geoserver_password,layername = layername, keyword = keyword)
#   }
#   getit_updatelayers(getit_url, getit_superuser = getit_superuser, 
#                      getit_password = getit_password, 
#                      workspacename = workspacename, 
#                      datastorename = datastore_postgis_name, 
#                      layername = layertitle)
#   
# }

# # qui sotto lo scompongo per analizzarlo
# #SERVONO, oltre ai parametri -F, anche i cookie, che dovrebbero essere già mantenuti da httr dopo il blocco di codice per l'autenticazione.
# # Va a questo aggiunto 
#    header X-CSRFToken: 9OThoSuzNNx9H1oohNjkw61og5ltJdP8
# # SI POSSONO AGGIUNGERE (pare, da codice python) anche:
# # layer_title (in alternativa all'uso del filename)
# # abstract (altrimenti scrive: "no abstract provided")
# # 
# curl -v http://tellmehub.get-it.it/layers/upload 
# -H 'Accept: */*' 
# -F base_file=@body_of_water_canals_bbx_12.300568_45.4052_12.403564_45.462057.zip
# -F permissions='{"users":{"AnonymousUser":["view_resourcebase","download_resourcebase"]},"groups":{}}'
# -F zip_file=@body_of_water_canals_bbx_12.300568_45.4052_12.403564_45.462057.zip
# -F charset='UTF-8' 
# -H 'X-CSRFToken: 9OThoSuzNNx9H1oohNjkw61og5ltJdP8'
# -H 'Origin: http://tellmehub.get-it.it'
# -H 'Connection: keep-alive'
# -H 'Referer: http://tellmehub.get-it.it/layers/upload'
# -H 'Cookie: csrftoken=9OThoSuzNNx9H1oohNjkw61og5ltJdP8; _ga=GA1.2.952619385.1568619305; sessionid=428mb28rbx5rvo4kig9hoxthteowkmwe' 
# > test_curl.htm

