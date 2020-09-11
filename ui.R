
## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
   # menuItem("Top Species", tabName = "TopSp", icon = icon("fish")),
   # menuItem("Capture animation", tabName = "AnimCapture", icon = icon("fish"))
    menuItem("Home",tabName="Home"),
    menuItem("Visualisation", tabName = "Visualisation", icon = icon("bar-chart-o")),
    menuItem("Top10", tabName = "Top10", icon = icon("trophy")),
    menuItem("Data", tabName = "Data", icon = icon("table"))
  #  menuItem("Select", tabName = "Select", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
    tabItems(
    #tabItem(tabName = "TopSp",TopSpeciesUI(id = "id_2")),
    tabItem(tabName = "Home",HomeUI()),
    tabItem(tabName = "Visualisation",SubsetUI(id="id_3"),
              TimeChartUI(id = "id_4")),
    tabItem(tabName = "Top10",RaceChartUI(id="id_5")),
    tabItem(tabName = "Data", DataTableWideUI(id = "id_2"))
    #tabItem(tabName = "AnimCapture",AnimCaptUI(id = "id_2"))
    #  tabItem(tabName = "Select",SelectUI(id = "id_2"))
    )
  )

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "FAO Capture Dashboard",tags$li(class = "dropdown",
                                                     tags$a(href="https://www.blue-cloud.org", target="_blank", 
                                                            tags$img(height = "30px", alt="SNAP Logo", src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png")
                                                     )
  )),
  sidebar,
  body
)

