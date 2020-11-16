###Module
# Function for module UI
InfoBoxUI <- function(id) {
  ns <- NS(id)
            fluidPage(
             fluidRow(infoBoxOutput(ns("nb_region"),width=4),
                      infoBoxOutput(ns("nb_flag"),width=4),
                      infoBoxOutput(ns("nb_area"),width=4)),
             fluidRow(infoBoxOutput(ns("nb_year"),width=4),
                      infoBoxOutput(ns("nb_species"),width=4),
                      infoBoxOutput(ns("nb_capture"),width=4)
                      )
            )
           
}

# # Function for module server logic

#test
InfoBox <- function(input, output, session,data) {
  observe({


nb_region<-length(unique(data()$region))
nb_flag<-length(unique(data()$flag))
nb_area<-length(unique(data()$area))
nb_year<-length(unique(data()$year))
nb_species<-length(unique(data()$species))
nb_capture<-sum(data()$capture)
nb_capture<-if(nchar(nb_capture)>=13){paste0(round(nb_capture/10^9,2)," Gt")
}else if(nchar(nb_capture)>=7){paste0(round(nb_capture/10^6,2)," kt")
}else if(nchar(nb_capture)>=3){paste0(round(nb_capture/10^3,2)," mt")
}else{paste0(round(nb_capture,2)," t")}
output$nb_region<-renderInfoBox({
  infoBox(
    "Number of Region",
    nb_region,
    icon = icon("globe")
  )
})

output$nb_flag<-renderInfoBox({
  infoBox(
    "Number of Flagstates",
    nb_flag,
    icon = icon("flag")
  )
})

output$nb_area<-renderInfoBox({
  infoBox(
    "Number of Area",
    nb_area,
    icon = icon("anchor")
  )
})

output$nb_year<-renderInfoBox({
  infoBox(
    "Number of Year",
    nb_year,
    icon = icon("hourglass-half")
  )
})

output$nb_species<-renderInfoBox({
  infoBox(
    "Number of Species",
    nb_species,
    icon = icon("fish")
  )
})

output$nb_capture<-renderInfoBox({
  infoBox(
    "Sum of capture",
    nb_capture,
    icon = icon("fish")
  )
})

})
}

