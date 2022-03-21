library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(DT)

options(shiny.maxRequestSize = 30*1024^2)
`%nin%` = Negate(`%in%`)
library(fresh)
custom_colors_theme <- create_theme(
  bs4dash_color(
    lightblue = "#136377",
    olive = "#d8bc66",
    lime = "#fcec0c",
    orange = "#978d01",
    maroon = "#58482c",
    gray_x_light = "#d1c5c0"
  )
)

hud_service_data <- read.csv("https://raw.githubusercontent.com/gwenbeebe/CHIP_HMIS/main/Publishing/NHSDC_ByNameList/SupplementalData_ServiceGroups.csv") %>%
  left_join(read.csv("https://raw.githubusercontent.com/gwenbeebe/CHIP_HMIS/main/Publishing/NHSDC_ByNameList/SupplementalData_Services.csv"), 
            by = "RecordType") %>%
  dplyr::mutate(ServiceType = case_when(
    str_detect(Description, fixed("outreach", ignore_case=TRUE)) |
      Description == "Bed night" ~ "Homeless",
    str_detect(Description, fixed("rental", ignore_case=TRUE)) |
      str_detect(Description, fixed("eviction prevention", ignore_case=TRUE)) |
      str_detect(Description, fixed("security deposit", ignore_case=TRUE))~ "Housed"
  ))

##  variable assignment
{
  homeless_situations <- c(1, 2, 16, 18)
  housed_situations <- c(3, 10, 11, 14, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34, 35, 36)
  homeless_program_types <- c(1, 2, 4, 8)
  housing_program_types <- c(3, 9, 10, 55, 13)
  file_count <- 9 
  }