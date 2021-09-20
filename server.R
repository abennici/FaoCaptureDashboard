
server <- function(input, output, session) {

  show_modal_spinner(spin = "flower", color = "#112446",
                     text = "Data loading....please wait", session = shiny::getDefaultReactiveDomain()) # show the modal window
  #data<-callModule(module = QueryInfo, id = "id_1")#With WFS service
  data<-callModule(module = DataSource, id = "id_1")#Without WFS service
  results<-reactiveValues()
  observe({
  if(!is.null(data$data)&&!is.null(data$dsd)){
  remove_modal_spinner()
  }
  })
  
  observeEvent(input$sidebarmenu, {
    if(input$sidebarmenu == "Summary"){
      callModule(module = RaceChart,id="top",reactive(data$data))
      callModule(module = InfoBox,id="info",reactive(data$data),results=results)
    }})
  #data_subset<-callModule(module = Subset, id = "id_3",reactive(data$data))
  
  
  observeEvent(input$sidebarmenu, {
    if(input$sidebarmenu == "Visualization"){
      callModule(module = AreaChart,id="area",reactive(data$data),results=results)  
      callModule(module = SpChart,id="sp",reactive(data$data),results=results) 
      callModule(module = FlagChart,id="flag",reactive(data$data),results=results)
      callModule(module = PieMarChart,id="marine",reactive(data$data))
      callModule(module = PieInChart,id="inland",reactive(data$data))
      callModule(module = DownloadReport,id="report",reactive(data$md),reactive(data$query),reactive(data$param),results=results, reactive(data$data))
    }})
  
  observeEvent(input$sidebarmenu, {
    if(input$sidebarmenu == "Data"){
  callModule(module = DataTableWide,id="id_2",reactive(data$data),reactive(data$dsd))
  }})
}

#?pid=fao_capture_flag_dbquery_advanced_multiyear&layer=fao_capture_dbquery_layer_adv_multiyear&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&feature_geom=false&strategy=ogc_viewparams&par=flag:FRA+ITA;year:2016+2017+2018;aggregation_method:none&srs=EPSG:4326&dsd=%5B%7B"name":"Flagstate","definition":"Flagging%20country%20of%20the%20fishing%20vessels.","primitiveCode":"flag","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Species","definition":"Species%20captured,%20based%20the%20ASFIS%20list%20of%20species%20for%20fishery%20statistics%20purposes.","primitiveCode":"species","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Area","definition":"FAO%20Major%20area%20for%20statistical%20purposes","primitiveCode":"f_area","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Capture","definition":"Quantity%20of%20fish%20in%20number%20or%20biomass%20harvested%20in%20a%20given%20stratum","primitiveCode":"capture","primitiveType":"xsd:decimal","columnType":"variable","minOccurs":1,"maxOccurs":1,"uom":"t","uomLabel":"metric_ton"%7D,%7B"name":"Type%20of%20water","definition":"Marine%20or%20inland%20area","primitiveCode":"f_area_type","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Year","definition":"Year","primitiveCode":"year","primitiveType":"xsd:int","columnType":"attribute","minOccurs":1,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"geometry","definition":null,"primitiveCode":"geometry","primitiveType":"gml:MultiPolygonPropertyType","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Aggregation%20method","definition":"Method%20of%20aggregation","primitiveCode":"aggregation_method","primitiveType":"xsd:string","columnType":"attribute","minOccurs":1,"maxOccurs":1,"uom":null,"uomLabel":null%7D%5D
#?pid=fao_capture_flag_dbquery_advanced_multiyear&layer=fao_capture_dbquery_layer_adv_multiyear&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&feature_geom=false&strategy=ogc_viewparams&par=region:005+013+029;aggregation_method:none&srs=EPSG:4326&dsd=%5B%7B"name":"Flagstate","definition":"Flagging%20country%20of%20the%20fishing%20vessels.","primitiveCode":"flag","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Species","definition":"Species%20captured,%20based%20the%20ASFIS%20list%20of%20species%20for%20fishery%20statistics%20purposes.","primitiveCode":"species","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Area","definition":"FAO%20Major%20area%20for%20statistical%20purposes","primitiveCode":"f_area","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Capture","definition":"Quantity%20of%20fish%20in%20number%20or%20biomass%20harvested%20in%20a%20given%20stratum","primitiveCode":"capture","primitiveType":"xsd:decimal","columnType":"variable","minOccurs":1,"maxOccurs":1,"uom":"t","uomLabel":"metric_ton"%7D,%7B"name":"Type%20of%20water","definition":"Marine%20or%20inland%20area","primitiveCode":"f_area_type","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Year","definition":"Year","primitiveCode":"year","primitiveType":"xsd:int","columnType":"attribute","minOccurs":1,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"geometry","definition":null,"primitiveCode":"geometry","primitiveType":"gml:MultiPolygonPropertyType","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Aggregation%20method","definition":"Method%20of%20aggregation","primitiveCode":"aggregation_method","primitiveType":"xsd:string","columnType":"attribute","minOccurs":1,"maxOccurs":1,"uom":null,"uomLabel":null%7D%5D