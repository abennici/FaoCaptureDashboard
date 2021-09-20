####Extraction of the metadata associated to data to retrieve the complete labels, type of variable and units of measures
getColumnDefinitions = function(fc) {
  do.call("rbind",lapply(fc$featureType[[1]]$carrierOfCharacteristics,function(x){data.frame(MemberCode=ifelse(!is.null(x$code),x$code,""),
                                                                                             MemberName=ifelse(!is.null(x$memberName$value),x$memberName$value,""),
                                                                                             MemberType=ifelse(!is.null(x$valueType$aName$attrs[['xlink:href']]),x$valueType$aName$attrs[['xlink:href']],""),
                                                                                             PrimitiveType=ifelse(!is.null(x$valueType$aName$value),sub(".*:", "", x$valueType$aName$value),""),
                                                                                             MinOccurs=ifelse(!is.null(x$cardinality$range$lower),x$cardinality$range$lower,""),
                                                                                             MaxOccurs=ifelse(!is.null(x$cardinality$range$upper$value),x$cardinality$range$upper$value,""),
                                                                                             Definition=ifelse(!is.null(x$definition),x$definition,""),
                                                                                             MeasureUnitSymbol=ifelse(!is.null(x$valueMeasurementUnit$identifier$value),x$valueMeasurementUnit$identifier$value,""),
                                                                                             MeasureUnitName=ifelse(!is.null(x$valueMeasurementUnit$name$value),x$valueMeasurementUnit$name$value,""))}))
}  


DataSource <- function(input, output, session) {
  data <- reactiveValues(
    data=NULL,dsd=NULL,query=NULL,md=NULL,param=NULL
  )

 #   
#     region1 <-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_GROUPS.csv") 
#   #https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_GEOREGION.csv
#   region1<-subset(region1,select=c(ISO3_Code,Region_Group))
#   names(region1)<-c("flag","name")
#   region2<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_GEOREGION.csv") 
#   region2 <-subset(region2,select=c(Name_En,UN_Code))
#   names(region2)<-c("name","region")
#   region_register_github<-merge(region1,region2,by="name",all.x=T,all.y=F)
#   region_register_github<- region_register_github[c("flag", "region")]
# 
# # Capture data
# #Upload filesand merge
# #setwd("D:/FAO-BLUECLOUD_04052020_11082020/02-R/04-Github/FaoCaptureDashboard")
# 
# fao<-gsheet2tbl("https://docs.google.com/spreadsheets/d/18vInGz0ofHZdk-g4_nXjjygk7ffZH_KJ_Be9pu4gijA/edit?usp=sharing")
# fao <- fao[,!sapply(colnames(fao), startsWith, "S")]
# colnames(fao)[1:4] <- c("flag", "species", "f_area", "unit")
# fao <- fao[fao$unit == "Q_tlw",] 
# fao <- fao[!is.na(fao$flag),] 
# fao <- as.data.frame(fao)
# 
# fao_n <- reshape(fao, idvar = c("flag", "species", "f_area", "unit"), times = colnames(fao)[5:ncol(fao)], varying = list(5: ncol(fao)), v.names = "capture", direction = "long")
# fao_n$f_area_type <- ifelse(sprintf("%02d", fao_n$f_area)%in%c("01","02","03","04","05","06"),"inland","marine")
# fao_n$year <- as.integer(substring(fao_n$time, 2, nchar(fao_n$time)-1))
# fao_n<-fao_n %>% 
#       left_join(region_register_github)
# 
# #Remove time start and time end column
# fao_n<-subset(fao_n,select=-c(unit,time))
# fao_n<- fao_n[c("flag", "region", "species","f_area","capture","f_area_type","year")]
# data$data<-fao_n
  full_data<-readr::read_csv("./data/sdilab_geoflow_fao_capture.csv")
observe({
  query <- parseQueryString(session$clientData$url_search)
  
  pid <- if (!is.null(query$pid)){
    as.character(query$pid)
  }else{
    NULL
  }
  
  csw_server <-if (!is.null(query$csw_server)){
    as.character(query$csw_server)
  }else{
    NULL
  }
  
  csw_version <-if (!is.null(query$csw_version)){
    as.character(query$csw_version)
  }else{
    "2.0.2"
  }
  
  dsd<-if (!is.null(query$dsd)){
    #jsonlite::fromJSON(URLdecode(query$dsd))
    jsonlite::fromJSON(query$dsd)
  }else{
    NULL
  }
  
  par<-if (!is.null(query$par)){
    as.character(query$par)
  }else{
    NULL
  }
  
  #Connect to OGC CSW Catalogue to get METADATA
  CSW <- CSWClient$new(
    url = csw_server,
    serviceVersion = csw_version,
    logger = "INFO"
  )
  
  #Get metadata for dataset 'fao_capture_flag_dbquery_advanced_multiyear'
  md <- CSW$getRecordById(pid, outputSchema = "http://www.isotc211.org/2005/gmd")
  data$md<-md
  print("DataSource : md")
  print(md)
  
  
  if(is.null(par)){
 
        data$data<-full_data
        
  }else{
   
  list_par<-unlist(strsplit(par,";"))
  print(list_par)
  #regexp :"[+]"for split +
  #regexp : "\\s+" for split space
  #regexp : "[[:punct:][:space:]]+" for split space and +
  select_flag<-unlist(strsplit(sub(".*flag:", "", list_par[str_detect(list_par,"flag:")]),"[[:punct:][:space:]]+")) 
  select_flag<-if(!is.null(select_flag)){select_flag}else{unique(full_data$flag)}
  data$param$select_flag<-select_flag
  print(select_flag)
  select_region<-unlist(strsplit(sub(".*region:", "", list_par[str_detect(list_par,"region:")]),"[[:punct:][:space:]]+")) 
  select_region<-if(!is.null(select_region)){select_region}else{unique(full_data$region)}
  print(select_region)
  data$param$select_region<-select_region
  select_species<-unlist(strsplit(sub(".*species:", "", list_par[str_detect(list_par,"species:")]),"[[:punct:][:space:]]+")) 
  select_species<-if(!is.null(select_species)){select_species}else{unique(full_data$species)}
  print(select_species)
  data$param$select_species<-select_species
  select_f_area<-unlist(strsplit(sub(".*f_area:", "", list_par[str_detect(list_par,"f_area:")]),"[[:punct:][:space:]]+")) 
  select_f_area<-if(!is.null(select_f_area)){select_f_area}else{unique(full_data$f_area)}
  print(select_f_area)
  data$param$select_f_area<-select_f_area
  select_f_area_type<-unlist(strsplit(sub(".*f_area_type:", "", list_par[str_detect(list_par,"f_area_type:")]),"[[:punct:][:space:]]+")) 
  select_f_area_type<-if(!is.null(select_f_area_type)){select_f_area_type}else{unique(full_data$f_area_type)}
  print(select_f_area_type)
  data$param$select_f_area_type<-select_f_area_type
  select_year<-unlist(strsplit(sub(".*year:", "", list_par[str_detect(list_par,"year:")]),"[[:punct:][:space:]]+"))
  select_year<-if(!is.null(select_year)){select_year}else{unique(full_data$year)}
  print(select_year)
  data$param$select_year<-select_year
  
   data$data<-full_data%>%
              filter(flag %in% select_flag &
              region %in% select_region &
              species %in% select_species &
              f_area %in% select_f_area &
              f_area_type %in% select_f_area_type &
              year %in% select_year)
  
  print("DATA START")
  print(data$data)
  print("DATA END")
  }
  names(dsd)<-c("MemberName","Definition","MemberCode","PrimitiveType","MemberType","MinOccurs","MaxOccurs","MeasureUnitSymbol","MeasureUnitName")
  print(dsd)
  data$dsd<-dsd
  print(data$data)
  data$query<-query
  
})
return(data)
}







