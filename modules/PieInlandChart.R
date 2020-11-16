###Module
# Function for module UI
PieInChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  #tabPanel("Time", 
           
             box(title="Inland Species Repartition",width = 4,collapsible = F,
                 plotlyOutput(ns('pie_inland'),height="230px")%>%withSpinner(type = 2)
            
           )
 # )
}
# Function for module server logic
PieInChart <- function(input, output, session,data) {
  
  observe({
    output$pie_inland  <- renderPlotly({
        df_donut_inland <- data() %>%
          filter(f_area_type=="inland")%>%
          group_by(species) %>% 
          summarise(capture = sum(capture)) %>%
          left_join(sp_register)
        
        #fig <- plot_ly(df, labels = ~as.factor(label), values = ~capture, type = 'pie',textinfo = 'none')
        fig_donut_inland  <- plot_ly(df_donut_inland, labels = ~as.factor(label), values = ~capture,textinfo = 'none')
        fig_donut_inland  <- fig_donut_inland  %>% add_pie(hole = 0.6)
        fig_donut_inland  <- fig_donut_inland  %>% layout(title = '',
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          showlegend = FALSE)   
        
        
        
        
        
        return(fig_donut_inland)
    })
})
}