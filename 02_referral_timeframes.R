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


Referrals <- 
  read_excel(file.choose()) 


##  this is the PM Dashboard Enrollments report
Enrollments <- 
  read_excel(file.choose()) %>%
  # get program data
  left_join(read_excel(file.choose()), by = "ProgramID") %>%
  filter(ProgramType %in% c("PH - Rapid Re-Housing", "PH - Permanent Supportive Housing (disability required for entry)",
                     "PH – Housing Only", "PH – Housing with Services (no disability required for entry)"),
         `Head of Household?` == "Yes")


referral_data <- Referrals %>%
  left_join(Enrollments, by = "ClientID") %>%
  filter(as.Date(`Referral Date`) <= as.Date(EnrollDate),
         as.Date(`Referral Date`) + 365 >= as.Date(EnrollDate),
         (Result != "Not Attained" | is.na(Result)),
         ProviderName != "High-Risk Non-Congregate Shelter - CES") %>%
  group_by(ServiceID) %>%
  arrange(EnrollDate) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(days_to_move_in = as.integer(ymd(DateOfMoveIn) - ymd(`Referral Date`)),
         days_to_enroll = as.integer(ymd(EnrollDate) - ymd(`Referral Date`))) %>%
  select(ClientID, `Referral Date`, ProviderName, Result, ProgramName, ProgramID, EnrollDate, DateOfMoveIn, days_to_move_in, days_to_enroll)
  

referral_data %>%
  mutate(refer_year = year(`Referral Date`)) %>%
  group_by(refer_year) %>%
  summarise(Referrals = n(), 
            MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)),
            MedianDays = median(days_to_move_in, na.rm = TRUE),
            MedianEnrollDays = median(days_to_enroll))


temp <- referral_data %>%
  mutate(refer_year = year(`Referral Date`)) %>%
  filter(refer_year %in% c(2018,2020)) %>%
  left_join(Demographics, by = "ClientID") %>%
  filter(HUDEthnicity == "Hispanic/Latino") %>%
  group_by(refer_year, HUDEthnicity) %>%
  summarise(Referrals = n(), 
            MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)),
            MedianDays = median(days_to_move_in, na.rm = TRUE),
            MedianEnrollDays = median(days_to_enroll))

  
Enrollments %>%
  mutate(enroll_year = year(EnrollDate)) %>%
  filter(enroll_year >= 2017) %>%
  group_by(enroll_year) %>%
  summarise(Enrollments = n(), 
            MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)))


referral_data %>%
  mutate(refer_year = year(`Referral Date`)) %>%
  filter(refer_year == 2020) %>%
  group_by(ProgramName, ProgramID) %>%
  summarise(Referrals = n(), 
            MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)), 
            MedianDays = median(days_to_move_in, na.rm = TRUE))


timelines <- 
  read_excel(file.choose()) 

all_data <- timelines %>%
  left_join(Demographics, by = "ClientID")

all_data %>%
  mutate(refer_year = year(ServiceDate)) %>%
  group_by(refer_year) %>%
  summarise(MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)), 
            MedianDays = median(ReferralToHoused, na.rm = TRUE))

temp <- all_data %>%
  mutate(refer_year = year(ServiceDate)) %>%
  group_by(refer_year, RaceDesc) %>%
  summarise(MoveIns = sum(if_else(is.na(DateOfMoveIn), 0, 1)), 
            MedianDays = median(ReferralToHoused, na.rm = TRUE))
