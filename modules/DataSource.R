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
    data=NULL,dsd=NULL,query=NULL
  )

  fao <- readr::read_csv("./data/sdilab_geoflow_fao_capture.csv")
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
  data$data<-readr::read_csv("./data/sdilab_geoflow_fao_capture.csv")
observe({
  query <- parseQueryString(session$clientData$url_search)
  print(query$dsd)
  dsd<-if (!is.null(query$dsd)){
    #jsonlite::fromJSON(URLdecode(query$dsd))
    jsonlite::fromJSON(query$dsd)
  }else{
    NULL
  }
  names(dsd)<-c("MemberName","Definition","MemberCode","PrimitiveType","MemberType","MinOccurs","MaxOccurs","MeasureUnitSymbol","MeasureUnitName")
  print(dsd)
  data$dsd<-dsd
  data$query<-query
})
return(data)
}







