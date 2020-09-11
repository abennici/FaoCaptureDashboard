###Module
# Function for module UI
HomeUI <- function() {

  tabPanel(
    p(),
  h3("Welcome to the FAO Global Capture Monitoring"),
  p("This Dashboard is a use case propose of the ",a(href = "https://github.com/abennici/FaoCaptureDashboard", "template")," shiny dashboard tool for Open Fair Viewer."),
  p("The concept of this application is to provide a data visualization and analysis tool based on the R code and on the Shiny package directly interfaceable with the OpenFairViewer viewer."),
  p("The data sourcing is operate by the WFS service via the ",a(href = "https://github.com/eblondel/ows4R","ows4R"), "package. The server query is thus passed to the application by the url parameter."),
  p("This development is a demonstrator ingnitative of the European",a(href="https://www.blue-cloud.org/","BlueCloud project"),"."),
  p("Interested by this application ? You can find the source code ",a(href = "https://github.com/abennici/FaoCaptureDashboard", "here"),".")
)

}
