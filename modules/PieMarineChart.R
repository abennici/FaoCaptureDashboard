###Module
# Function for module UI
PieMarChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  #tabPanel("Time", 
           
             box(title="Marine Species Repartition",width = 4,collapsible = F,
                 plotlyOutput(ns('pie_marine'),height="230px")%>%withSpinner(type = 2)
             
           )
 # )
}
# Function for module server logic
PieMarChart <- function(input, output, session,data) {
  
  observe({
    #DONUT Chart_marine
    output$pie_marine  <- renderPlotly({
      df_donut_marine <- data() %>% 
        filter(f_area_type=="marine")%>%
        group_by(species) %>%
        summarise(capture = sum(capture)) %>%
        left_join(sp_register)
      
      #fig <- plot_ly(df, labels = ~as.factor(label), values = ~capture, type = 'pie',textinfo = 'none')
      fig_donut_marine  <- plot_ly(df_donut_marine, labels = ~as.factor(label), values = ~capture,textinfo = 'none')
      fig_donut_marine  <- fig_donut_marine  %>% add_pie(hole = 0.6)
      fig_donut_marine  <- fig_donut_marine  %>% layout(title = '',
                                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                        showlegend = FALSE)
      
      return(fig_donut_marine)
    })
})
}