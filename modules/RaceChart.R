###Module
# Function for module UI
RaceChartUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

  tabPanel("RaceChart", 
         fluidRow(box(imageOutput(ns('Top_sp'))%>%withSpinner(type = 2)),
                  box(imageOutput(ns('Top_flag'))%>%withSpinner(type = 2))
                      
                  )
           )
 
}

# # Function for module server logic

#test
RaceChart <- function(input, output, session,data) {
  observe({

tab<-as.data.frame(data())

data <- tab%>%
  left_join(l_sp,by="species") %>%
  group_by(year,sp_name) %>% 
  summarise(capture = sum(capture))%>%
  group_by(year) %>%
  mutate(rank = rank(-capture)) %>%
  group_by(sp_name) %>% 
  filter(rank <=10) %>%
  ungroup()
print(data)

p <- data %>%
  ggplot(aes(x = -rank,y = capture, group = sp_name,fill=sp_name)) +
  geom_tile(aes(y = capture / 2, height = capture), width = 0.9) +
  #geom_text(aes(label = sp_name), hjust = "right", colour = "black",  size=4, fontface = "italic", nudge_y = -100000) +
  geom_text(aes(y = 0, label = sp_name), vjust = 0.2, hjust = 1) +
  geom_text(aes(label = scales::comma(capture)), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  scale_x_reverse()+
  #scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
   guides(color = FALSE, fill = FALSE) +
         theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="none",
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
             panel.grid.major.x = element_line( size=.1, color="grey" ),
             panel.grid.minor.x = element_line( size=.1, color="grey" ),
             plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
             plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
             plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
             plot.background=element_blank(),
             plot.margin = margin(2,2, 2, 4, "cm"))+
  transition_time(year) +
  view_follow(fixed_x = TRUE)  +
 # ease_aes('cubic-in-out') +
  labs(title='Top 10 species',
       subtitle='Capture in {round(frame_time,0)}',
       caption="Fish capture in Tones | Data Source: FAO")

output$Top_sp <- renderImage({
  #outfile <- tempfile(fileext='.gif')
  anim_save("outfile.gif", animate(p, fps = 5, end_pause =40, width = 450, height = 300))
  # nframes = length(unique(data$year))*6
  list(src = "outfile.gif",
       contentType = 'image/gif'
  )}, deleteFile = TRUE)

###FLAG Chart

  data_flag <- tab%>%
    left_join(l_flag,by="flag") %>%
    group_by(year,flag_name) %>% 
    summarise(capture = sum(capture))%>%
    group_by(year) %>%
    mutate(rank = rank(-capture)) %>%
    group_by(flag_name) %>% 
    filter(rank <=10) %>%
    ungroup()

  p2 <- data_flag %>%
    ggplot(aes(x = -rank,y = capture, group = flag_name,fill=flag_name)) +
    geom_tile(aes(y = capture / 2, height = capture), width = 0.9) +
    geom_text(aes(y = 0, label = flag_name), vjust = 0.2, hjust = 1) +
    geom_text(aes(label = scales::comma(capture)), hjust = "left", nudge_y = 100000, colour = "grey30") +
    coord_flip(clip="off") +
   # scale_x_discrete("") +
    scale_x_reverse()+
    scale_y_continuous("",labels=scales::comma) +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm"))+
    transition_time(year) +
    view_follow(fixed_x = TRUE)  +
  #  ease_aes('cubic-in-out') +
    labs(title='Top 10 flag',
         subtitle='Capture in {round(frame_time,0)}',
         caption="Fish capture in Tones | Data Source: FAO")
  
  output$Top_flag <- renderImage({
    #outfile <- tempfile(fileext='.gif')
    anim_save("outfile2.gif", animate(p2, fps = 5, end_pause =40, width = 450, height = 300))
    # nframes = length(unique(data$year))*6
    list(src = "outfile2.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
})

  }

# AnimCapt <- function(input, output, session,data) {
#   observe({
#     
#     library(geoflow)
#     library(plyr)
#     library(dplyr)
#     library(ggplot2)
#     library(gganimate)
#     setwd("D://sandbox-geoflow")
#     my_dbi <- dbConnect(drv=Postgres(),
#                         user= "sdilab_u",
#                         password= "48076d497555f98",
#                         host= "sdilab-geo-db.d4science.org",
#                         port= "5432",
#                         dbname= "sdilab")
#     fao_n<-st_read(dsn = my_dbi,query = "SELECT * FROM sdilab_geoflow_fao_capture")
#     country<-st_read(dsn = my_dbi,query = "SELECT geometry,iso_3 FROM countries")
#     fao<-merge(fao_n,country,by.x="flag",by.y="iso_3",all.x=T,all.y=F)
#     write.csv(fao,"fao_capture_test.csv", row.names = FALSE)
#     dbDisconnect(my_dbi)
#     head(fao)
#     
#     fao2<-ddply(fao,.(flag,year),summarize,capture=sum(capture))
#     
#     head(fao2)
#     
#     #Formating data
#     
#     fao2_formatted <- fao2 %>%
#       group_by(year) %>%
#       # The * 1 makes it possible to have non-integer ranks while sliding
#       mutate(rank = rank(-capture)) %>%
#       group_by(flag) %>% 
#       filter(rank <=10) %>%
#       ungroup()
#     
#     #plot
#     staticplot = ggplot(fao2_formatted, aes(rank, group = flag, 
#                                             fill = as.factor(flag), color = as.factor(flag))) +
#       geom_tile(aes(y = capture/2,
#                     height = capture,
#                     width = 0.9), alpha = 0.8, color = NA) +
#       geom_text(aes(y = 0, label = paste(flag, " ")), vjust = 0.2, hjust = 1) +
#       geom_text(aes(y=capture,label = round(capture,0), hjust=0)) +
#       coord_flip(clip = "off", expand = FALSE) +
#       scale_y_continuous(labels = scales::comma) +
#       scale_x_reverse() +
#       guides(color = FALSE, fill = FALSE) +
#       theme(axis.line=element_blank(),
#             axis.text.x=element_blank(),
#             axis.text.y=element_blank(),
#             axis.ticks=element_blank(),
#             axis.title.x=element_blank(),
#             axis.title.y=element_blank(),
#             legend.position="none",
#             panel.background=element_blank(),
#             panel.border=element_blank(),
#             panel.grid.major=element_blank(),
#             panel.grid.minor=element_blank(),
#             panel.grid.major.x = element_line( size=.1, color="grey" ),
#             panel.grid.minor.x = element_line( size=.1, color="grey" ),
#             plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
#             plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
#             plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
#             plot.background=element_blank(),
#             plot.margin = margin(2,2, 2, 4, "cm"))
#     
#     #Animate plot
#     anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
#       view_follow(fixed_x = TRUE)  +
#       labs(title = 'Capture per Year : {closest_state}',  
#            subtitle  =  "Top 10 Countries",
#            caption  = "Fish capture in Tones | Data Source: FAO")
#     
#     #Rendering
#     animate(anim,duration=60, fps = 10,  width = 1200, height = 1000,start_pause = 3,end_pause = 4,
#             renderer = gifski_renderer("gganim.gif",loop = F))
#     #########################################################
#     #Global
#     fao3<-ddply(fao,.(year),summarize,capture=sum(capture))
#     ## Global annual change
#     global_annual_change <- ggplot(fao3, aes(year, capture)) +
#       geom_line(col = "black", size = 1.4, alpha = 0.6) +
#       ## geom_point needs a year based group so that it renders correctly in the gif
#       geom_point(size = 1.6, alpha = 1, col = "black", aes(group = seq_along(year))) + 
#       scale_y_continuous() +
#       theme_minimal() +
#       labs(
#         y = "Annual capture in tones",
#         x = "Year",
#         title = "Global capture per year",
#         caption = "Fish capture in Tones | Data Source: FAO"
#       ) + 
#       ##gganimate code
#       transition_reveal(year)
#     
#     ## Animate the gif
#     animate(global_annual_change,duration=15, fps = 10,  width = 1200, height = 1000,start_pause = 3,end_pause = 4,
#             renderer = gifski_renderer("gganim-global.gif",loop = F))
#     
#     ## Print
#     global_annual_change_gif
#     
#     #####################################
#     #Region
#     #########################################################
#     #Global
#     fao3<-ddply(fao,.(year),summarize,capture=sum(capture))
#     ## Global annual change
#     global_annual_change <- ggplot(fao3, aes(year, capture)) +
#       geom_line(col = "black", size = 1.4, alpha = 0.6) +
#       ## geom_point needs a year based group so that it renders correctly in the gif
#       geom_point(size = 1.6, alpha = 1, col = "black", aes(group = seq_along(year))) + 
#       scale_y_continuous() +
#       theme_minimal() +
#       labs(
#         y = "Annual capture in tones",
#         x = "Year",
#         title = "Global capture per year",
#         caption = "Fish capture in Tones | Data Source: FAO"
#       ) + 
#       ##gganimate code
#       transition_reveal(year)
#     
#     ## Animate the gif
#     animate(global_annual_change,duration=15, fps = 10,  width = 1200, height = 1000,start_pause = 3,end_pause = 4,
#             renderer = gifski_renderer("gganim-global.gif",loop = F))
#     
#     
#     ##################
#     #Region
#     fao4<-ddply(fao,.(f_area,year),summarize,capture=sum(capture))
#     ## Global annual change
#     region_annual_change <- ggplot(fao4, aes(year, capture,col=f_area)) +
#       geom_line(size = 1.4, alpha = 0.6) +
#       ## geom_point needs a year based group so that it renders correctly in the gif
#       geom_point(size = 1.6, alpha = 1, col = "black", aes(group = seq_along(year))) + 
#       scale_y_continuous() +
#       facet_wrap(~f_area, scales = "free_y") +
#       theme_minimal() +
#       labs(
#         y = "Annual capture in tones",
#         x = "Year",
#         title = "Area capture per year",
#         caption = "Fish capture in Tones | Data Source: FAO"
#       ) + 
#       ##gganimate code
#       transition_reveal(year)
#     
#     ## Animate the gif
#     animate(region_annual_change,duration=15, fps = 10,  width = 1200, height = 1000,start_pause = 3,end_pause = 4,
#             renderer = gifski_renderer("gganim-region.gif",loop = F))
#     
#     