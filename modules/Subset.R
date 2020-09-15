###Module
# Function for module UI
SubsetUI <- function(id) {
  ns <- NS(id)
  
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Subset", fluidRow(
    sliderInput(ns("time"),"Choose Period",min=0,max=1,value=c(0,1)),
    selectizeInput(ns("flag"),"Choose Flags",c(""), multiple = TRUE),
    selectizeInput(ns("species"),"Choose Species",c(""), multiple = TRUE),
    checkboxGroupInput(ns("type"),"Choose Type of Area",c("")),

    tags$div(actionButton(ns("gobutton"),"Update selection"))

  ))
  
}




# Function for module server logic
Subset <- function(input, output, session,data,dsd) {
  
  data_subset<-reactiveValues(
    data=NULL
  )
  observe({
    tab<-data()
    class(tab) <- "data.frame"
    print("Here we are")
    print(sprintf("We have selected %s rows in data", nrow(data_subset$data)))
    
      #Time
    year <- as.integer(unique(tab$year))
    if (is.null(year))
      year<- character(0)
    #Flag
    flag <- unique(tab$flag)
    if (is.null(flag))
      flag<- character(0)
    #Species 
    species <- unique(tab$species)
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
    
    data_subset$data<-tab
  })  
  ###Reformat
  observeEvent(input$gobutton, {
    select_year<-if(is.null(input$time))unique(data()$year)else seq(min(input$time),max(input$time),1)
    select_flag<-if(is.null(input$flag))unique(data()$flag)else(input$flag)
    select_species<-if(is.null(input$species))unique(data()$species)else(input$species)
    select_type<-if(is.null(input$type))unique(data()$f_area_type)else(input$type)
    #cat("type:")
    #print(select_type)
    #print(input$type)
    # data_subset$data<-subset(data(),year %in% select_year &
    #                                        flag %in% select_flag &
    #                                        species %in% select_species &
    #                                        f_area_type %in% select_type)
    
    data_subset$data<-data()%>%
                      filter(year %in% select_year &
                             flag %in% select_flag &
                          species %in% select_species &
                      f_area_type %in% select_type)
 
    
    print(data_subset$data)
    
  })
  
  return(data_subset)
  
}
####