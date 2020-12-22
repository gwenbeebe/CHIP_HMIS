library(tidyverse)
library(lubridate)
library(readxl)
library(pander)
library(dplyr)

directory <- "Data Requests/data"
ReportStart <- ymd("20191001")
ReportEnd <- ymd("20201001")

Demographics <- 
  read_excel(paste0(directory, "/Demographics - 12.1.17 - 12.17.20.xlsx")) %>%
  mutate(RaceDesc = if_else(RaceDesc %in% c("Data not collected", "Client doesn't know", "Client refused") |
                                 is.na(RaceDesc), "Unknown", RaceDesc))

Programs <- 
  read_excel(paste0(directory, "/Programs 12.16.20.xlsx")) 

Enrollments <- 
  read_excel(paste0(directory, "/Enrollments 12.1.17 - 11.30.20.xlsx")) 

all_data <- Enrollments %>%
  left_join(Demographics, by = "ClientID") %>%
  left_join(Programs, by = "ProgramID")

rm(list = ls()[!(ls() %in% c("all_data"))])

save.image("images/all_data.RData")


active_in_period <- all_data %>%
  filter(EnrollDate < ReportEnd &
           (is.na(ExitDate) | ExitDate >= ReportStart))

diversion_households_active_in_period <- active_in_period %>%
  filter((ProgramName == "Diversion" 
          | ProgramName == "IHN - Diversion"
          | ProgramName == "YHDP - Diversion")
         & `Head of Household?` == "Yes")  %>%
  group_by(ClientID) %>%
  arrange(EnrollDate) %>%
  slice(1L) %>%
  ungroup()

diversion_households_enhanced <- diversion_households_active_in_period %>%
  left_join(all_data %>%
              filter(ProgramType == "Emergency shelter") %>%
              select("ClientID", "EnrollDate"),
            by = "ClientID") %>%
  group_by(ClientID) %>%
  mutate(entered_shelter = if_else(EnrollDate.x <= EnrollDate.y, 1, 0),
         entered_shelter = if_else(is.na(max(entered_shelter)), 0, max(entered_shelter)),
         entered_shelter = if_else(entered_shelter == 1, "Yes", "No")) %>%
  select(-EnrollDate.y) %>%
  arrange(EnrollDate.x) %>%
  slice(1L) %>%
  ungroup()

# x <- table(diversion_households_enhanced$RaceDesc, diversion_households_enhanced$entered_shelter)
# prop.table(x)
# addmargins(x)
# summary(x)
# mosaicplot(x)