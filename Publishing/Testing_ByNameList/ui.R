shinyUI(
  dashboardPage(
    dashboardHeader(title = "By Name Lists"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenuid",
        menuItem("Data Upload",
                 menuSubItem("ZIP Upload", icon = icon("angle-double-right"), tabName = "zip_upload"),
                 menuSubItem("CSV Upload", icon = icon("angle-double-right"), tabName = "csv_upload")),
        menuItem("Veteran By Name List",
                 tabName = "VBNL")
      )),
    dashboardBody(
      shinyDashboardThemes(
        # theme = "poor_mans_flatly"
        theme = "grey_light"
      ),
      tabItems(
        tabItem(
          tabName = "zip_upload",
          # fluidRow(box(
          #   htmlOutput("headerPrioritization"), width = 12
          # )),
          # Upload zip files
          fluidRow(
            box(fileInput("file", "Upload zip file", accept = ".zip"))),
          fluidRow(
            # action button to unzip the file
            box(actionButton("unzip", "Unzip Files"), 
          ),
          
          # to display the metadata of the zipped file
          fluidRow(
            tabBox(
              title = "Upload Information",
              id = "zip_upload_info",
              tabPanel("Metadata", tableOutput("filedf")),
              # to display the list of unzipped files
              tabPanel("File List", tableOutput("zipped")), 
              tabPanel(textOutput("label"), tableOutput("test")),
              # to display affiliation
              tabPanel("Affiliation", tableOutput("affiliation"))
              , width = 12))
        )),
        tabItem(
          tabName = "csv_upload"
          )
      )
    )
  )
)