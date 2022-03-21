# Copyright (C) 2022 Gwen Beebe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 


shinyUI(
  dashboardPage(
    dashboardHeader(title = "By-Name List Generator"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenuid",
        menuItem("Tool Setup",
                 menuSubItem("Import HUD CSV", 
                             icon = icon("file-import"), tabName = "hud_import"),
                 menuSubItem("Optional Settings", 
                             icon = icon("gear", verify_fa = FALSE), tabName = "settings"),
                 startExpanded = TRUE),
        menuItem("By-Name Lists",
                 menuSubItem("Veteran By Name List",
                             icon = icon("flag-usa"), tabName = "VBNL"),
                 startExpanded = TRUE),
        menuItem("Tool Information",
                 menuSubItem("Tool Information",
                             icon = icon("info-circle"), tabName = "information"),
                 startExpanded = TRUE)
      )),
    dashboardBody(
      shinyDashboardThemes(
        theme = "poor_mans_flatly"
        # theme = "grey_light"
      ),
      tags$style(
        type = 'text/css',
        '.modal-dialog { width: fit-content !important; }'
      ),
      tabItems(
        tabItem(
          tabName = "hud_import",
          fluidPage(
            titlePanel("HUD CSV Import"),
            fluidRow(
              box(solidHeader = TRUE, title = "Import File Here",
                  fileInput("file", " ", accept = ".zip"), 
                  width = 12, status = "primary")),)),
        tabItem(
          tabName = "settings",
          fluidPage(
            titlePanel("By-Name List Settings"),
            fluidRow(
              box(title = "Days to Inactive",
                  numericInput("days_to_inactive", "", 90),
                  status = "primary", width = 4, solidHeader = TRUE
              ),
              box(
                status = "primary", width = 4
              ),
              box(
                status = "primary", width = 4
              )))),
        tabItem(
          tabName = "information",
          fluidPage(
            titlePanel("Tool Information"),
            fluidRow(box(
              title = "Why would I use this?", status = "primary", width = 12, solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              "The idea behind a by-name list is to help a CoC identify who is experiencing homelessness "
              ),
              box(
              title = "Is this secure?", status = "primary", width = 12, solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE, 
              "The safety and security of client information always comes first. Based on the following information provided by RStudio (the owner of shinyapps.io), I believe this is secure, but I am not a lawyer and final responsibility for our clients' information security lies with each of us. No client data is stored anywhere by my code, it exists only within the context of a given session.", br(), br(),
              blockQuote(
                "Each app is deployed into its own container, and the network access between containers is tightly controlled. All access to the apps is over SSL, and you can configure your app to require authentication prior to anyone accessing it if you have the Standard plan or above.", br(), br(),
                "The design of the system is for every account to have its own sub-directory structure, and to enforce the security at the file system and operating system levels. The storage for each container is not permanent, so if you need to store data, our strong recommendation is for you to push that data into your own data store.", br(), br(),
                "shinyapps.io is currently hosted on Amazon's Web Services (AWS) infrastructure in the us-east-1 region."),
              tags$a(href="https://docs.rstudio.com/shinyapps.io/security-and-compliance.html", "RStudio: Security and Compliance"), br(), br(),
              blockQuote("Each application deployed to shinyapps.io creates storage in its own private file system when the application starts to run. The application only has access to the data that was uploaded with the application at the time of deployment."),
              tags$a(href="https://docs.rstudio.com/shinyapps.io/Storage.html", "RStudio: Storage"), br(), br(),
              "If you would prefer to run this tool on your local machine to be on the safe side, you can download the code for the full project from ", 
              tags$a(href="https://github.com/gwenbeebe/CHIP_HMIS/tree/main/Publishing/NHSDC_ByNameList", "my Github"),
              "and run the whole thing completely offline. If you have any trouble at all getting set up, just let me know and we will work through it together."
            )),
            fluidRow(box(
              title = "How can I distribute and modify this?", status = "primary", solidHeader = TRUE, 
              collapsible = TRUE, collapsed = TRUE, width = 12,
              "This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.", br(), br(),
              "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ",
              tags$a(href="https://www.gnu.org/licenses/", "GNU Affero General Public License"),
              " for more details.", br(), br(),
              "This license does technically mean you", tags$a(href="https://www.gnu.org/philosophy/selling.en.html", "could sell it"), "but...the whole point is increasing capacity for CoCs that can't make this kind of technical investment in-house. Just be cool, man.")),
            fluidRow(box(
              title = "How can I help?", status = "primary", solidHeader = TRUE, 
              collapsible = TRUE, collapsed = TRUE, width = 12,
              "First and foremost, if you would like to modify and/or improve upon this tool (and I hope you do!) please feel free to create a Github branch and let me know what you learn. If you want to submit issues or merge branches, even better. Like anything else, this tool is a work in progress and can only benefit from having more eyes on it!", br(), br(),
              "Beyond that...if any of this has been helpful to you, please consider donating to the agencies that supported its development:", br(),
              tags$a(href="https://www.councilforthehomeless.org/donate-online/", "Council for the Homeless"), "for believing this was a worthwhile way to build by-name lists,", br(),
              tags$a(href="https://www.chipindy.org/donate.html", "CHIP"), "for letting me implement and improve this passion project,", br(),
              "or", tags$a(href="http://cohhio.org/donate/", "COHHIO"), " for showing us all what could be done with HMIS and R.")),
            fluidRow(box(
              title = "How do I send feedback?", status = "primary", solidHeader = TRUE, 
              collapsible = TRUE, collapsed = TRUE, width = 12,
              "If you're the techie type and you find an issue, go ahead and flag it on my Github! If that sounds confusing and/or intimidating (I promise it isn't as bad as it sounds), you can always email me at",
              tags$a(href="mailto:gwenbeebe@gmail.com", "gwenbeebe@gmail.com"), br(), br(),
              "Also please feel free to shoot me a note if this is even remotely useful to you! I'd love to hear about it.")),
            fluidRow(
              valueBox("Author", "Gwen Beebe", icon = icon("user-circle"), color = "navy", width = 6),
              valueBox("Last Updated", "3/20/22", icon = icon("calendar"), color = "navy", width = 6)
            ))),
        tabItem(
          tabName = "VBNL",
          fluidPage(
            titlePanel("Veteran By-Name List"),
            fluidRow(box(solidHeader = TRUE, status = "primary",
                         htmlOutput(
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
