###Module
# Function for module UI
TimeChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  tabPanel("Time", 
           fluidRow(
             box(title="By type of area",width = 4,collapsible = F,
                 plotlyOutput(ns('Plot_area'),height="250px")%>%withSpinner(type = 2)
             ),
             box(title="By species",width = 4,collapsible = F,
                 plotlyOutput(ns('Plot_sp'),height="250px")%>%withSpinner(type = 2)
             ),
             box(title="By flags",width = 4,collapsible = F,
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
TimeChart <- function(input, output, session,data) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  #AREA
 # observe({
    
  output$Plot_area <- renderPlotly({
    isolate({
    print("Loading df_area")
    
    df_area <- data() %>%
      group_by(year,f_area_type) %>% 
      summarise(capture = sum(capture))%>%
      accumulate_by(~year)%>%
      ungroup()
    #mutate(f_area_type,as.factor(f_area_type))
    
    print("df_area loaded")
    
    #AREA PLOT
    
    fig_area <- df_area %>% 
      plot_ly(
        x = ~year, 
        y = ~capture,
        #stackgroup = 'one',
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
        range = range(unique(df_area$frame)),
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
    
    
    fig_area <- fig_area %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
    
   # fig_area <- fig_area %>% animation_slider(active=length(unique(df_area$frame)),y = 0.2,anchor="middle",currentvalue = list(
   #    tickcolor='#ffffff',ticklength=0,prefix = "",font = list(size=10))
   #  )
  #  fig_area
    print("Figure for area prepared")
    
   
    
    print("Figure for area RENDERED")
   return(fig_area) 
    })
  })
  
  #observe({
    
    output$Plot_sp <- renderPlotly({
  #  df<-data()
    
    
    #SP PLOT
    
    print("Loading df_sp")

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
      accumulate_by(~year)%>%
      ungroup()
    #mutate(species,as.factor(species))
    
    print("df_sp loaded")
    
    print(sprintf("We have selected %s species!", length(unique(df_sp$species))))
    
    fig_sp <- df_sp %>% plot_ly()
    fig_sp<-fig_sp%>% add_trace(
      x = ~year, 
      y = ~capture,
      #stackgroup = 'one',
      split=~species,
      frame = ~frame,
      type = 'scatter', 
      mode = 'lines',
      line = list(simplyfy = F), 
      text = ~paste("Year: ", year, "<br>capture: ", capture), 
      hoverinfo = 'text'
    )
    fig_sp <- fig_sp %>% layout(
      legend = list(orientation = "h",
                    font = list(size = 10),
                    bgcolor ='rgba(0,0,0,0)'
                   # xanchor = "center",
                    #yanchor = "top",
                    #y =-0.1,
                    #x = 0.5)
                   ),
      xaxis = list(
        titlefont = list(size = 10), 
        tickfont = list(size = 10),
        range = range(unique(df_sp$frame)),
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
    
    
    fig_sp <- fig_sp %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
    
    print("Figure for species prepared")
    return(fig_sp)
    #output$Plot_sp <- renderPlotly(fig_sp)
  })
    
    output$Plot_flag <- renderPlotly({
    print("Figure for sp RENDERED")
    
    #FLAG PLOT
    print("Loading df_flag")
    
    rank_flag <- data() %>%
      group_by(flag) %>% 
      summarise(capture = sum(capture))%>%
      mutate(rank = rank(-capture)) %>%
      filter(rank <=10) %>%
      ungroup()
    
    df_flag <- data() %>%
      filter(flag%in%rank_flag$flag)%>%
      group_by(year,flag) %>% 
      summarise(capture = sum(capture))%>%
      accumulate_by(~year)%>%
      ungroup()
    #mutate(flag,as.character(flag))
    
    print("df_flag loaded")
    
    fig_flag <- df_flag %>% plot_ly()
    fig_flag<-fig_flag%>% add_trace(
      x = ~year, 
      y = ~capture,
      #stackgroup = 'one',
      split=~flag,
      frame = ~frame,
      type = 'scatter', 
      mode = 'lines',
      line = list(simplyfy = F),
      text = ~paste("Year: ", year, "<br>capture: ", capture), 
      hoverinfo = 'text'
    )
    fig_flag <- fig_flag %>% layout(
      legend = list(orientation = "h",
                    font = list(size = 10),
                    bgcolor ='rgba(0,0,0,0)'
                    # xanchor = "center",
                    #yanchor = "top",
                    #y =-0.1,
                    #x = 0.5)
      ),
      xaxis = list(
        titlefont = list(size = 10), 
        tickfont = list(size = 10),
        range = range(unique(df_flag$frame)),
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
    
    
    fig_flag <- fig_flag %>% animation_opts(frame = 50,transition = 50,redraw = FALSE )
    # fig_flag <- fig_flag %>% layout(title = "",
    #                                 yaxis = list(title = "Capture (Tons) by Flags", autorange = TRUE,automargin=TRUE, zeroline = F),
    #                                 xaxis = list( title = "Year", autorange = TRUE,automargin=TRUE,zeroline = F, showgrid = F)
    # ) 
    # fig_flag <- fig_flag %>% animation_opts(frame = 100,transition = 0,redraw = FALSE )
    # 
    # fig_flag <- fig_flag %>% animation_slider(y = 0.2,anchor="middle",currentvalue = list(
    #   active=2018,tickcolor='#ffffff',ticklength=0,prefix = "",font = list(size=10))
    # )
    
    print("Figure for flag prepared")
    
    
      return(fig_flag)
    
    print("Figure for flag RENDERED")
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