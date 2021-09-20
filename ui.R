
## ui.R ##
sidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 25px}"),
  width=100,
  sidebarMenu(id="sidebarmenu",
   # menuItem("Home",tabName="Home"),
    #menuItem("Query", tabName = "Query"),#, icon = icon("gears")
    menuItem("Visualization", tabName = "Visualization"),#, icon = icon("bar-chart-o")
    menuItem("Summary", tabName = "Summary"),#, icon = icon("trophy")
    menuItem("Data", tabName = "Data")#, icon = icon("table")
  )
)

body <- dashboardBody(
  #tags$head(tags$style(HTML('.info-box {min-height: 30px;} .info-box-icon {height: 30px; width: 30px; line-height: 30px;    font-size: 30px;} .info-box-content {padding-top: 0px;padding-left: 0px; padding-bottom: 0px;}  .info-box-text {font-size: 10px;} .info-box-number {font-size: 14px;}'))),
  tags$head(tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;    font-size: 40px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}  .info-box-text {font-size: 15px;} .info-box-number {font-size: 20px;}'))),
  tabItems(
   tabItem(tabName = "Home",HomeUI()),
   # tabItem(tabName = "Query",SubsetUI(id="id_3")),
    tabItem(tabName = "Summary",InfoBoxUI(id="info"),RaceChartUI(id="top")),
    tabItem(tabName = "Visualization",
            fluidRow(
              AreaChartUI(id = "area"),
              SpChartUI(id = "sp"),
              FlagChartUI(id = "flag")
              ),
            fluidRow(
              PieMarChartUI(id = "marine"),
              PieInChartUI(id = "inland"),
              DownloadReportUI(id = "report")
              )
            ),
    tabItem(tabName = "Data", DataTableWideUI(id = "id_2"))
    )
  )

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(titleWidth = 0,title = "",tags$li(class = "dropdown",
                                                    tags$style(".main-header {max-height: 25px}"),
                                                    tags$style(".main-header .logo {height: 25px;}"),
                                                    tags$style(".sidebar-toggle {height: 25px; padding-top: 1px !important;}"),
                                                    tags$style(".navbar {min-height:25px !important}"),
                                                    tags$img(height = "20px", alt="SNAP Logo", src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png")
  )),
  sidebar,
  body
)

