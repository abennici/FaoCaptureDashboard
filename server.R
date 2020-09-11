
server <- function(input, output, session) {
  
  data<-callModule(module = QueryInfo, id = "id_1")
  data_subset<-callModule(module = Subset, id = "id_3",reactive(data$data))
  callModule(module = TimeChart,id="id_4",reactive(data_subset$data))
  callModule(module = RaceChart,id="id_5",reactive(data$data))
  callModule(module = DataTableWide,id="id_2",reactive(data_subset$data),reactive(data$dsd))
  
  

}