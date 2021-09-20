###Module
# Function for module UI
DataTableWideUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  tabPanel("Data", 
           #downloadButton(ns('downloadData'), "Download"),
           div(DTOutput(ns('table'))%>%withSpinner(type = 2),  style = "font-size:80%"))
  
}


# Function for module server logic
DataTableWide <- function(input, output, session,data,dsd) {
  observe({
    ###Reformat
    tab<-as.data.frame(data())
   # tab<-subset(tab,select=-c(geometry))
    name<-data.frame(MemberCode=names(tab))
    name<-name%>%left_join(dsd(),by="MemberCode")
    label<-paste0(name$MemberName," [",name$MemberCode,"]")
    tab$capture<-paste0(tab$capture," t")
    names(tab)<-label
    
    #output$table <- renderDataTable(
    output$table <- DT::renderDT(server = FALSE, {
      DT::datatable( 
                                  tab,
                                    #escape = FALSE,
                                    #rownames=FALSE,
                                    extensions = c("Buttons"), 
                                    options =list(
                                      dom = 'Bfrtip',
                                      pageLength=5,
                                      #lengthChange=FALSE,
                                      #deferRender = TRUE,
                                      scroll = FALSE,
                                      buttons = list(
                                        list(extend = 'copy'),
                                        list(extend = 'csv', filename =  paste0(Sys.Date(),"Fao_Capture_Data"), title = NULL, header = TRUE),
                                        list(extend = 'excel', filename =  paste0(Sys.Date(),"Fao_Capture_Data"), title = NULL, header = TRUE),
                                        list(extend = "pdf", filename = paste0(Sys.Date(),"Fao_Capture_Data"), title = "Fao_Capture_Data", header = TRUE),
                                        list(extend = 'print')
                                      ),
                                      exportOptions = list(
                                        modifiers = list(page = "all", selected = TRUE)
                                                          )
                                      )
                                    )
    
                         
})
  })
}
####