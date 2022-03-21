shinyUI(
  dashboardPage(
    dashboardHeader(title = "By Name Lists"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenuid",
        menuItem("By-Name List Setup",
                 menuSubItem("Import HUD CSV", 
                             icon = icon("file-import"), tabName = "hud_import"),
                 menuSubItem("Optional Settings", 
                             icon = icon("gear"), tabName = "settings"),
                 startExpanded = TRUE),
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
          tabName = "hud_import",
          fluidPage(
          titlePanel("HUD CSV Import"),
          # fluidRow(box(
          #   htmlOutput("headerPrioritization"), width = 12
          # )),
          # Upload zip files
          fluidRow(
            box(fileInput("file", "Import File Here", accept = ".zip"), 
                width = 12)),)),
        tabItem(
          tabName = "settings"
          ),
        tabItem(
          tabName = "VBNL",
          fluidPage(
            tags$style(
              type = 'text/css',
              '.modal-dialog { width: fit-content !important; }'
            ),
          titlePanel("Veteran By-Name List"),
          fluidRow(box(htmlOutput(
            "datesVBNL"
          ), width = 12)),
          fluidRow(
            infoBoxOutput("VBNL_active", width = 6),
            infoBoxOutput("VBNL_newly", width = 6)),
          fluidRow(
            infoBoxOutput("VBNL_return_h", width = 6),
            infoBoxOutput("VBNL_return_i", width = 6)),
          fluidRow(
            box(
              dataTableOutput("veteran_by_name_list"),
              width = 12))
        ))
      )
    )
  )
)
