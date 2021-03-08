library(tidyverse)
library(lubridate)
library(readxl)
library(pander)
library(dplyr)

directory <- "Data Requests/data"
ReportStart <- ymd("20191001")
ReportEnd <- ymd("20201001")

##  this is the Full Demographic Table report
Demographics <- 
  read_excel(file.choose()) %>%
  mutate(RaceDesc = if_else(RaceDesc %in% c("Data not collected", "Client doesn't know", "Client refused") |
                                 is.na(RaceDesc), "Unknown", RaceDesc),
         HUDEthnicity = if_else(HUDEthnicity %in% c("Data not collected", "Client doesn't know", "Client refused") |
                                             is.na(HUDEthnicity), "Unknown", HUDEthnicity))
Programs <- 
  read_excel(file.choose()) 

##  this is the PM Dashboard Enrollments report
Enrollments <- 
  read_excel(file.choose()) 

all_data <- Enrollments %>%
  left_join(Demographics, by = "ClientID") %>%
  left_join(Programs, by = "ProgramID")

rm(list = ls()[!(ls() %in% c("all_data"))])

save.image("images/all_data.RData")
