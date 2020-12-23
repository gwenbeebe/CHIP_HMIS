library(tidyverse)
library(lubridate)
library(readxl)
library(pander)
library(dplyr)

directory <- "Data Requests/data"
ReportStart <- ymd("20191001")
ReportEnd <- ymd("20201001")

Demographics <- 
  read_excel(paste0(directory, "/Demographics - 12.1.17 - 12.17.20 v2.xlsx")) %>%
  mutate(RaceDesc = if_else(RaceDesc %in% c("Data not collected", "Client doesn't know", "Client refused") |
                                 is.na(RaceDesc), "Unknown", RaceDesc),
         HUDEthnicity = if_else(HUDEthnicity %in% c("Data not collected", "Client doesn't know", "Client refused") |
                                             is.na(HUDEthnicity), "Unknown", HUDEthnicity))
Programs <- 
  read_excel(paste0(directory, "/Programs 12.16.20.xlsx")) 

Enrollments <- 
  read_excel(paste0(directory, "/Enrollments 12.1.17 - 11.30.20.xlsx")) 

all_data <- Enrollments %>%
  left_join(Demographics, by = "ClientID") %>%
  left_join(Programs, by = "ProgramID")

rm(list = ls()[!(ls() %in% c("all_data"))])

save.image("images/all_data.RData")
