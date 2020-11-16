###Module
# Function for module UI
SpChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))

             box(width = 4,collapsible = F,
                 radioButtons(ns("Plot_sp_select"),"By species",choices = c("Default", "Animation"),selected = "Default",inline = TRUE),
                 plotlyOutput(ns('Plot_sp'),height="250px")%>%withSpinner(type = 2)
             
           
  )
}
# Function for module server logic
SpChart <- function(input, output, session,data) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
observe({
  
  rank_sp <- data() %>%
    group_by(species) %>% 
    summarise(capture = sum(capture))%>%
    mutate(rank = rank(-capture)) %>%
    filter(rank <=10) %>%
    ungroup()
  
  df_sp <- data() %>%
    filter(species %in% rank_sp$species)%>%
    group_by(year,species) %>% 
    summarise(capture = sum(capture))%>%
    ungroup
  
  observeEvent(input$Plot_sp_select, {
    if(input$Plot_sp_select == "Default"){ 
      print("DEFAULT")
      fig_sp <- df_sp %>% plot_ly()
      fig_sp<-fig_sp%>% add_trace(
        x = ~year, 
        y = ~capture,
        split=~species,
        type = 'scatter', 
        mode = 'lines',
        line = list(simplyfy = F),
        text = ~paste("Year: ", year, "<br>capture: ", capture, "<br>species: ", species), 
        hoverinfo = 'text'
      )
      
      fig_sp <- fig_sp %>% layout(
        legend = list(orientation = "h",
                      font = list(size = 10),
                      bgcolor ='rgba(0,0,0,0)',
                      xanchor = "center",
                      yanchor = "top",
                      y =-0.1,
                      x = 0.5),
        xaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          range = range(unique(df_sp$year)),
          title = "Year",
          zeroline = F
        ),
        yaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          range = range(df_sp$capture),
          title = "Capture (Tons)",
          zeroline = F
        ))
      
      return(output$Plot_sp <- renderPlotly({fig_sp}))
      
    }else{      
      print("ANIMATE")
      df_sp2<-df_sp%>%
        accumulate_by(~year)%>%
        ungroup()
      
      print("df_sp loaded")
      
      fig_sp <- df_sp2%>%plot_ly()
      fig_sp<-fig_sp%>% add_trace(
        x = ~year, 
        y = ~capture,
        split=~species,
        frame = ~frame,
        type = 'scatter', 
        mode = 'lines',
        line = list(simplyfy = F),
        text = ~paste("Year: ", year, "<br>capture: ", capture, "<br>species: ", species), 
        hoverinfo = 'text'
      )
      
      fig_sp <- fig_sp %>% layout(
        legend = list(orientation = "h",
                      font = list(size = 10),
                      bgcolor ='rgba(0,0,0,0)',
                      xanchor = "center",
                      yanchor = "top",
                      y =-0.1,
                      x = 0.5),
        xaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          range = range(unique(df_sp2$frame)),
          title = "Year",
          zeroline = F
        ),
        yaxis = list(
          titlefont = list(size = 10), 
          tickfont = list(size = 10),
          range = range(df_sp2$capture),
          title = "Capture (Tons)",
          zeroline = F
        ))
      
      
      fig_sp<- fig_sp %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
      return(output$Plot_sp <- renderPlotly({fig_sp}))
    }
  })
  
  
})
}