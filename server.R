
server <- function(input, output, session) {
  show_modal_spinner(spin = "flower", color = "#112446",
                     text = "Data loading....please wait", session = shiny::getDefaultReactiveDomain()) # show the modal window
  data<-callModule(module = QueryInfo, id = "id_1")
  observe({
  if(!is.null(data$data)){
  remove_modal_spinner()}
  })
  data_subset<-callModule(module = Subset, id = "id_3",reactive(data$data))
  observeEvent(input$sidebarmenu, {
  if(input$sidebarmenu == "Visualisation"){
    callModule(module = TimeChart,id="id_4",reactive(data_subset$data))  
}})
  callModule(module = RaceChart,id="id_5",reactive(data$data))
  callModule(module = DataTableWide,id="id_2",reactive(data_subset$data),reactive(data$dsd))
  
}