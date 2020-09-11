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
               plotlyOutput(ns('Plot_area'))%>%withSpinner(type = 2)
               ),
             box(title="By species",width = 4,collapsible = F,
                 plotlyOutput(ns('Plot_sp'))%>%withSpinner(type = 2)
             ),
             box(title="By flags",width = 4,collapsible = F,
                 plotlyOutput(ns('Plot_flag'))%>%withSpinner(type = 2)
             )
           ),
           fluidRow(
             box(title="Marine Species Repartition",width = 6,collapsible = F,
                 plotlyOutput(ns('pie_marine'))%>%withSpinner(type = 2)
             ),
             box(title="Inland Species Repartition",width = 6,collapsible = F,
                 plotlyOutput(ns('pie_inland'))%>%withSpinner(type = 2)
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
  
  observe({
    
    df<-as.data.frame(data())
    
    df_area <- df %>%
      group_by(year,f_area_type) %>% 
      summarise(capture = sum(capture))%>%
      accumulate_by(~year)%>%
      ungroup()
      #mutate(f_area_type,as.factor(f_area_type))
    
    df_sp <- df %>%group_by(year,species) %>% 
      summarise(capture = sum(capture))%>%
      accumulate_by(~year)%>%
      ungroup()
      #mutate(species,as.factor(species))
    
    df_flag <- df %>%group_by(year,flag) %>% 
      summarise(capture = sum(capture))%>%
      accumulate_by(~year)%>%
      ungroup()
      #mutate(flag,as.character(flag))
    
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
    
      fig_area <- fig_area %>% layout(title = "",
      yaxis = list(title = "Capture (Tons) by Type of Area", range = c(0,max(df$capture)+25*max(df$capture)/100), zeroline = F),
      xaxis = list( title = "Year", zeroline = F)
    ) 
    fig_area <- fig_area %>% animation_opts(frame = 100,transition = 0,redraw = FALSE )
    
    fig_area <- fig_area %>% animation_slider(y = 0.2,anchor="middle",currentvalue = list(
        active=2018,tickcolor='#ffffff',ticklength=0,prefix = "",font = list(size=10))
          )
  
  
    #SP PLOT
    
    fig_sp <- df_sp %>% plot_ly(height = 300)
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
    
    fig_sp <- fig_sp %>% layout(title = "",
                                    yaxis = list(title = "Capture (Tons) by Species", range = c(0,max(df$capture)+25*max(df$capture)/100), zeroline = F),
                                    xaxis = list( title = "Year", zeroline = F, showgrid = F)
    ) 
    fig_sp <- fig_sp %>% animation_opts(frame = 100,transition = 0,redraw = FALSE )
    
    fig_sp <- fig_sp %>% animation_slider(y = 0.2,anchor="middle",currentvalue = list(
      active=2018,tickcolor='#ffffff',ticklength=0,prefix = "",font = list(size=10))
    )    
    
    #FLAG PLOT
    
    fig_flag <- df_flag %>% plot_ly(height = 300)
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
    
    fig_flag <- fig_flag %>% layout(title = "",
                                yaxis = list(title = "Capture (Tons) by Flags", range = c(0,max(df$capture)+25*max(df$capture)/100), zeroline = F),
                                xaxis = list( title = "Year", zeroline = F, showgrid = F)
    ) 
    fig_flag <- fig_flag %>% animation_opts(frame = 100,transition = 0,redraw = FALSE )
    
    fig_flag <- fig_flag %>% animation_slider(y = 0.2,anchor="middle",currentvalue = list(
      active=2018,tickcolor='#ffffff',ticklength=0,prefix = "",font = list(size=10))
    )
    
  #DONUT Chart_marine

    df_donut_marine <- df %>% 
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
    #DONUT Chart_inland
    
    df_donut_inland <- df %>%
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

    output$Plot_area <- renderPlotly(fig_area)
    output$Plot_sp <- renderPlotly(fig_sp)
    output$Plot_flag <- renderPlotly(fig_flag)
    output$pie_marine  <- renderPlotly(fig_donut_marine)
    output$pie_inland  <- renderPlotly(fig_donut_inland)
  })
}
##