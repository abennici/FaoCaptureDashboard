###Module
# Function for module UI
FlagChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  #tabPanel("Time", 
           
             box(width = 4,collapsible = F,
                 radioButtons(ns("Plot_flag_select"),"By flags",choices = c("Default", "Animation"),selected = "Default",inline = TRUE),
                 plotlyOutput(ns('Plot_flag'),height="250px")%>%withSpinner(type = 2)
             
           )
 # )
}
# Function for module server logic
FlagChart <- function(input, output, session,data) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  observe({
    
    rank_flag <- data() %>%
      group_by(flag) %>% 
      summarise(capture = sum(capture))%>%
      mutate(rank = rank(-capture)) %>%
      filter(rank <=10) %>%
      ungroup()
    df_flag <- data() %>%
      filter(flag %in% rank_flag$flag)%>%
      group_by(year,flag) %>% 
      summarise(capture = sum(capture))%>%
      ungroup
    
    observeEvent(input$Plot_flag_select, {
      if(input$Plot_flag_select == "Default"){ 
        print("DEFAULT")
        fig_flag <- df_flag %>% 
          plot_ly(
            x = ~year, 
            y = ~capture,
            split=~flag,
            type = 'scatter', 
            mode = 'lines',
            line = list(simplyfy = F),
            text = ~paste("Year: ", year, "<br>capture: ", capture, "<br>flag: ", flag), 
            hoverinfo = 'text'
          )
        
        fig_flag <- fig_flag %>% layout(
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
            range = range(unique(df_flag$year)),
            title = "Year",
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(df_flag$capture),
            title = "Capture (Tons)",
            zeroline = F
          ))
        
        
        return(output$Plot_flag <- renderPlotly({fig_flag}))
      }else{      
        print("ANIMATE")
        df_flag2<-df_flag%>%
          accumulate_by(~year)%>%
          ungroup()
        
        print("df_flag loaded")
        
        fig_flag <- df_flag2 %>% 
          plot_ly(
            x = ~year, 
            y = ~capture,
            split=~flag,
            frame = ~frame,
            type = 'scatter', 
            mode = 'lines',
            line = list(simplyfy = F),
            text = ~paste("Year: ", year, "<br>capture: ", capture, "<br>flag: ", flag), 
            hoverinfo = 'text'
          )
        
        fig_flag <- fig_flag %>% layout(
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
            range = range(unique(df_flag2$frame)),
            title = "Year",
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(df_flag2$capture),
            title = "Capture (Tons)",
            zeroline = F
          ))
        
        
        fig_flag<- fig_flag %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
        return(output$Plot_flag <- renderPlotly({fig_flag}))
      }
    
    
  })  
  
  
})
}