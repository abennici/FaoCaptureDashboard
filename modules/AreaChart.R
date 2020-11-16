###Module
# Function for module UI
AreaChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))

             box(width = 4,collapsible = F,
                 radioButtons(ns("Plot_area_select"),"By type of area",choices = c("Default", "Animation"),selected = "Default",inline = TRUE),
                 plotlyOutput(ns('Plot_area'),height="250px")%>%withSpinner(type = 2)
          
  )
}

# Function for module server logic
AreaChart <- function(input, output, session,data) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  observe({
    
    df_area <- data() %>%
      group_by(year,f_area_type) %>% 
      summarise(capture = sum(capture))%>%
      ungroup
    
    observeEvent(input$Plot_area_select, {
      if(input$Plot_area_select == "Default"){ 
        print("DEFAULT")
        fig_area <- df_area %>% 
          plot_ly(
            x = ~year, 
            y = ~capture,
            split=~f_area_type,
            type = 'scatter', 
            mode = 'lines',
            line = list(simplyfy = F),
            text = ~paste("Year: ", year, "<br>capture: ", capture), 
            hoverinfo = 'text'
          )
        
        fig_area <- fig_area %>% layout(
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
            range = range(unique(df_area$year)),
            title = "Year",
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(df_area$capture),
            title = "Capture (Tons)",
            zeroline = F
          ))
        
        
        return(output$Plot_area <- renderPlotly({fig_area}))
      }else{      
        print("ANIMATE")
        df_area2<-df_area%>%
          accumulate_by(~year)%>%
          ungroup()
        
        print("df_area loaded")
        
        #AREA PLOT
        
        fig_area <- df_area2 %>% 
          plot_ly(
            x = ~year, 
            y = ~capture,
            split=~f_area_type,
            frame = ~frame,
            type = 'scatter', 
            mode = 'lines',
            line = list(simplyfy = F),
            text = ~paste("Year: ", year, "<br>capture: ", capture), 
            hoverinfo = 'text'
          )
        
        fig_area <- fig_area %>% layout(
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
            range = range(unique(df_area2$frame)),
            title = "Year",
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(df_area2$capture),
            title = "Capture (Tons)",
            zeroline = F
          ))
        
        
        fig_area <- fig_area %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
        return(output$Plot_area <- renderPlotly({fig_area}))
      }
    })
    
    
  })
}
##