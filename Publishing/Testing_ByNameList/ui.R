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
        theme = "poor_mans_flatly"
        # theme = "grey_light"
      ),
      tags$style(HTML(".box.box-solid.box-primary>.box-header {}
                .box.box-solid.box-primary{background:#222d32}
                .box.box-solid.box-warning>.box-header {}
                .box.box-solid.box-warning{background:#222d32}
                .box.box-solid.box-danger>.box-header {
                color:#ffffff; background:MistyRose}
                .box.box-solid.box-danger{
                  background:MistyRose;
                  border-bottom-color:LightPink;
                  border-left-color:LightPink;
                  border-right-color:LightPink;
                  border-top-color:LightPink;
                }"),
        type = 'text/css',
        '.modal-dialog { width: fit-content !important; }'
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
          tabName = "settings",
          fluidPage(
            fluidRow(
              box(width = 6, title = "test", status = "primary", solidHeader 
                  = TRUE,
                  "Box content"
              )
            ),
            fluidRow(
              box(width = 6, title = "XXX Veterans", status = "warning", solidHeader 
                  = TRUE,
                  "Are newly homeless"
              )
            ),
            fluidRow(
              box(width = 3, title = tags$b(textOutput("VBNL_active_text")), 
                  status = "danger", solidHeader 
                  = TRUE,
                  "Are newly homeless"
              )
            )
          )
          ),
        tabItem(
          tabName = "VBNL",
          fluidPage(
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
