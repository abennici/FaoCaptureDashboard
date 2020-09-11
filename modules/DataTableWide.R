###Module
# Function for module UI
DataTableWideUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  tabPanel("Data", downloadButton(ns('downloadData'), "Download"),
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
    
    output$table <- renderDT(tab,rownames='',options =list(pageLength=5,lengthChange=FALSE))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(tab, file, row.names = FALSE)
      }
    )
                                 
})
}
####