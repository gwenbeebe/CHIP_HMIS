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


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(DT)
library(shinydashboardPlus)
library(colourpicker)

options(shiny.maxRequestSize = 30*1024^2)
`%nin%` = Negate(`%in%`)

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