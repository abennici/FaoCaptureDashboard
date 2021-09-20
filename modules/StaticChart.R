###Module
# Function for module UI
StaticChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  tabPanel("Time", 
           fluidRow(
             box(width = 4,collapsible = F,
                radioButtons(ns("Plot_area_select"),"By types of area",choices = c("Static", "Dynamic"),selected = "Static",inline = TRUE),
                 plotlyOutput(ns('Plot_area'),height="250px")%>%withSpinner(type = 2)
             ),
             box(width = 4,collapsible = F,
			 radioButtons(ns("Plot_sp_select"),"By species",choices = c("Static", "Dynamic"),selected = "Static",inline = TRUE),
                 plotlyOutput(ns('Plot_sp'),height="250px")%>%withSpinner(type = 2)
             ),
             box(width = 4,collapsible = F,
			 radioButtons(ns("Plot_flag_select"),"By flags",choices = c("Static", "Dynamic"),selected = "Static",inline = TRUE),
                 plotlyOutput(ns('Plot_flag'),height="250px")%>%withSpinner(type = 2)
             )
           ),
           fluidRow(
             box(title="Marine Species Repartition",width = 6,collapsible = F,
                 plotlyOutput(ns('pie_marine'),height="230px")%>%withSpinner(type = 2)
             ),
             box(title="Inland Species Repartition",width = 6,collapsible = F,
                 plotlyOutput(ns('pie_inland'),height="230px")%>%withSpinner(type = 2)
             )
           )
  )
}

# Function for module server logic
StaticChart <- function(input, output, session,data) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  #AREA
   observe({
  
        df_area <- data() %>%
        group_by(year,f_area_type) %>% 
        summarise(capture = sum(capture))%>%
          ungroup
        
        observeEvent(input$Plot_area_select, {
          if(input$Plot_area_select == "Static"){ 
            print("STATIC")
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
          print("DYNAMIC")
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
###SPECIES  
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
          if(input$Plot_sp_select == "Static"){ 
            print("SP_STATIC")
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
          print("DYNAMIC")
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

#FLAGS
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
          if(input$Plot_flag_select == "Static"){ 
            print("STATIC")
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
          print("DYNAMIC")
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
  #DONUT Chart_inland
  output$pie_inland  <- renderPlotly({
    isolate({
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
##