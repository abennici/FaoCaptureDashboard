#ui
DownloadReportUI <- function(id) {
  ns <- NS(id)
  tabBox(width = 4,height="290px",
tabPanel("Global Report",downloadButton(ns('createReport'), 'Download Report'),
         textInput(ns('global_subtitle_text'),label = 'Add a Subtitle to report',value = "")),
tabPanel("Regionalized Report", 
         downloadButton(ns('createReportRegion'), 'Download Regionized Report'),
         textInput(ns('regionalized_subtitle_text'),label = 'Add a Subtitle to report',value = ""),
         uiOutput(ns('region_selection')))
)

}
#server
DownloadReport <- function(input, output, session,md,query,param,results,data) {
  ns<-session$ns
Sys.setlocale('LC_TIME', 'English') 
  observe({
    data<-data()
    md<-md()
    query<-query()
    param<-param()
    print(results)
    results$globalsubtitle<-input$global_subtitle_text
    results$regionalized_subtitle<-input$regionalized_subtitle_text
    
output$region_selection<-renderUI({
  region<-subset(region_register_github(),code %in% unique(data$region))$label
  selectInput(inputId = ns('region'), label = "Choose region to include to the report:", choices = region, selected = region, multiple=T)
})
results$region<-input$region
print(results$region)
createPDFReport <- function(file, results) {
#  tempReport <- file.path(tempdir(), "Visualization_Report_2.Rmd")
 # file.copy("markdown/Visualization_Report_2.Rmd", tempReport, overwrite = TRUE)
  params <- list(results = results,md = md, query = query , param = param, data = data)
  return (rmarkdown::render("markdown/Visualization_Report_2.Rmd", output_file = file, params = params))
}

output$createReport <- downloadHandler(
  filename = paste("Fao_Global_Capture_report_",format(Sys.time(), "%Y%m%d"),".pdf",sep=""),
  content = function(file) {
    createPDFReport(file,results)
  }
)

title<-paste0("Summary report of fisheries statistics of captures from ",unique(min(data$year)),"  to ",unique(max(data$year))," by regions and countries")
createPDFReport2 <- function(file, results) {
 # tempReport <- file.path(tempdir(), "main.Rmd")
  #file.copy("markdown/main.Rmd", tempReport, overwrite = TRUE)
  params <- list(md = md, query = query , param = param, data = data, title=title)
  return (rmarkdown::render("markdown/main.Rmd", output_file = file, params = params))
}

output$createReportRegion <- downloadHandler(
  filename = paste("Fao_Regionalized_Capture_report_",format(Sys.time(), "%Y%m%d"),".pdf",sep=""),
  content = function(file) {
    createPDFReport2(file,results)
  }
)



  })  
  # reportFileName <- paste(tempdir(),"/","Fao_Capture_report_",format(Sys.time(), "%Y%m%d"),".pdf",sep="")
  # createPDFReport(reportFileName,results)
}