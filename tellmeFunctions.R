# TELLme glossary functions
# @author: ptagliolato (tagliolato.p@irea.cnr.it)
require(SPARQL)
endpoint_url = "http://fuseki1.get-it.it/TELLmeGlossary/query"
## sparql queries

protocolsQuery<-function(){
  return("
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX tellme:<http://rdfdata.get-it.it/TELLmeGlossary/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    
    SELECT ?protocol ?protocol_id ?protocol_label
    WHERE{
      ?protocol a tellme:protocol;
      skos:prefLabel ?protocol_label.
      BIND(SUBSTR(STR(?protocol), 50) as ?protocol_id)
      FILTER(LANG(?protocol_label) = \"en\")  
    }")
}

#  lista concept_id, concept_label given a protocol (uri) and a scale 
# (come proprietà di contenimento)

conceptsByProtocolAndScaleQuery<-function(string_protocol_id,string_scale){
  qpattern<-"
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX tellme:<http://rdfdata.get-it.it/TELLmeGlossary/>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  
  SELECT ?keyword ?conceptSlug ?sintconcept ?conceptLabel ?c_p_s 
  WHERE {
  BIND(tellme:protocol_%s as ?protocollo)
  BIND(tellme:containsAtScale_%s as ?anyproperty)
  #  BIND(
  ?protocollo 
  skos:prefLabel ?protocolLabel;
  a tellme:protocol ;
  ?anyproperty ?relatedConcept .
  
  ?relatedConcept 
  skos:prefLabel ?conceptLabelEn;
  skos:broader/skos:prefLabel ?keyword .
  
  FILTER(LANG(?protocolLabel) =\"en\")
  FILTER(LANG(?conceptLabelEn) = \"en\")  
  FILTER(LANG(?keyword) = \"en\")  
  
  BIND(SUBSTR(STR(?relatedConcept), 49) as ?sintconcept)
  BIND(SUBSTR(STR(?protocollo), 50) as ?sintprotocol)
  BIND(SUBSTR(STR(?anyproperty), 49+8) as ?scale)
  BIND(xsd:integer(?sintconcept) as ?intconcept)
  BIND(xsd:integer(?sintprotocol) as ?intprotocol)
  BIND(concat(?protocolLabel,\"-\",?scale) as ?semanticPackage)
  
  BIND(CONCAT(\"concept_\",?sintconcept) as ?conceptSlug)
  BIND(CONCAT(\"c_\",?sintconcept, \"-p_\",?sintprotocol,\"-s_\",?scale ) as ?c_p_s)
  BIND(STR(?conceptLabelEn) as ?conceptLabel)
  
  }
  ORDER BY ?intprotocol ?scale ?keyword ?intconcept
  "
  q<-sprintf(qpattern,string_protocol_id, string_scale)
  return(q)
}

#id=11
conceptById<-function(id){
  qpattern<-'
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX tellme:<http://rdfdata.get-it.it/TELLmeGlossary/>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  
    SELECT * WHERE {
      BIND(tellme:concept_%s as ?relatedConcept)

      ?relatedConcept a skos:Concept, tellme:concept ;
        skos:prefLabel ?cLabel;
        skos:broader/skos:prefLabel ?keyword .
  
      FILTER(LANG(?cLabel) = "en")  
      FILTER(LANG(?keyword) = "en")  
      
  	  BIND(STR(?cLabel) as ?conceptLabel)
      BIND(SUBSTR(STR(?relatedConcept), 49) as ?sintconcept)
      BIND(CONCAT("concept_",?sintconcept) as ?conceptSlug)
    }
  '
  q<-sprintf(qpattern,id)
  return(q)
}

queries=list(
  protocols=protocolsQuery, 
  conceptsByProtocolAndScale=conceptsByProtocolAndScaleQuery,
  conceptById=conceptById
  )

#  dato (protocollo, scala) ottenere lista dei related concepts
#  per cui cercare i dati in OSM)

# queryFunctionName<-"conceptsByProtocolAndScaleQuery"
# params=list("1","XL")

# generic function
getDfByQuery<-function(queryFunctionName, params=list()){
  if(is.null(names(params))){
    names(params)<-formalArgs(queries[[queryFunctionName]])
  }
  qd <- SPARQL::SPARQL(url = endpoint_url, query =  do.call(queries[[queryFunctionName]],params) )
  df <- qd$results
  return(df)
}

# named functions
getProtocols<-function(){
  return(getDfByQuery("protocols"))
}

getConceptsByProtocolIdAndScale<-function(protocolId, scale){
  # #s_protocol_id<-df$protocol_id[1]
  # #s_protocol_id<-"1"
  # #s_scale<-"L" #"XL", "M", "S"...
  # q <- SPARQL::SPARQL(url= endpoint_url, 
  #                      query = queries$conceptsByProtocolAndScaleQuery(s_protocol_id,s_scale))
  # df<- q$results
  # return(df)
  return(getDfByQuery("conceptsByProtocolAndScale", list(protocolId, scale)))
}

getConceptById<-function(conceptId){
  return(getDfByQuery("conceptById",list(conceptId)))
}

# retrieve the concept label as currently stored as a HierarchicalKeyword in the TELLme Hub
TELLmeHub.getConceptCurrentLabel<-function(conceptId){
  urlsearchpattern="http://tellmehub.get-it.it/api/keywords/?slug__iexact=concept_%s"
  urlsearch<-sprintf(urlsearchpattern,conceptId)
  res<-httr::GET(urlsearch)
  rescontent<-httr::content(res)
  return(rescontent$objects[[1]]$name)
}

# TELLmeHub.getConceptCurrentLabel(11)
# getConceptById(11)$conceptLabel
