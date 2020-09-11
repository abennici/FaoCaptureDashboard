###Module
# Function for module UI
SubsetUI <- function(id) {
  ns <- NS(id)
  
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Subset", fluidRow(
    sliderInput(ns("time"),"Choose Period",min=0,max=1,value=c(0,1)),
    selectizeInput(ns("flag"),"Choose Flags",c(""), multiple = TRUE
                #   options = list(
                #     onInitialize = I('function() { this.setValue(""); }'))
                   ),
    selectizeInput(ns("species"),"Choose Species",c(""), multiple = TRUE
                #   options = list(
                #     onInitialize = I('function() { this.setValue(""); }'))
                   ),
    checkboxGroupInput(ns("type"),"Choose Type of Area",c("")
                #   options = list(
                #     onInitialize = I('function() { this.setValue(""); }'))
                   ),
    #uiOutput(width = 6, ns("selectFlag")),
    #uiOutput(width = 6, ns("selectSpecies")),
    tags$div(actionButton(ns("gobutton"),"Run"))
                      # imageOutput(ns('plot1'))
    ))
  
  
  
}




# Function for module server logic
Subset <- function(input, output, session,data,dsd) {
  data_subset<-reactiveValues(
   data=NULL
  )
  observe({
    data_subset$data<-data()
    tab<-as.data.frame(data())
    
    
    #Time
    year <- c(as.integer(unique(tab$year)))
    if (is.null(year))
      year<- character(0)
    #Flag
    flag <- c(unique(tab$flag))
    if (is.null(flag))
      flag<- character(0)
    #Species 
    species <- c(unique(tab$species))
    if (is.null(species))
      species<- character(0)
    #Type
    f_area_type <- unique(tab$f_area_type)
    if (is.null(f_area_type))
      f_area_type<- character(0)
  
   # Update YEAR
    updateSliderInput(session, 
                      inputId="time",
                      value = c(min(year),max(year)),
                      min = min(year), 
                      max = max(year),
                      step=1)
   # Update FLAG
    updateSelectizeInput(session,
                         inputId ="flag",
                         choices = unique(flag))
    
    
    # Update SPECIES
    updateSelectizeInput(session,
                         inputId ="species",
                         choices = unique(species)
                         )
    # Update TYPE OF AREA
    updateCheckboxGroupInput(session,
                         inputId ="type",
                         choices = unique(f_area_type),
                         inline = TRUE
                         )

  })  
  ###Reformat
  observeEvent(input$gobutton, {
    tmp<-data()
    select_year<-if(is.null(input$time))unique(tmp$year)else seq(min(input$time),max(input$time),1)
    select_flag<-if(is.null(input$flag))unique(tmp$flag)else(input$flag)
    select_species<-if(is.null(input$species))unique(tmp$species)else(input$species)
    select_type<-if(is.null(input$type))unique(tmp$f_area_type)else(input$type)
    cat("type:")
print(select_type)
#print(input$type)
     tab<-subset(tmp,year %in% select_year &
                     flag %in% select_flag &
                     species %in% select_species &
                     f_area_type %in% select_type)
                 
    #tab<-tab%>%filter(flag %in% select_flag &
    #                  species %in% select_species &
    #                  f_area_type %in% select_type)
    
print(tab)
#print(select_flag)
  data_subset$data<-tab
  }
)
  
  return(data_subset)
  
}
####