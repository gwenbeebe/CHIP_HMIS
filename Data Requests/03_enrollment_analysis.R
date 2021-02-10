#library(tidyverse)
#library(lubridate)
library(dplyr)
#library(readxl)

##  read in file
Enrollments <- 
  read_excel(file.choose())

assessment_data <- Enrollments %>%
  filter(ProgramType == "Coordinated Entry") %>%
  setNames(paste("CE", colnames(Enrollments), sep = "_"))

housing_program_data <- Enrollments %>%
  filter(ProgramType %in% c("PH - Permanent Supportive Housing (disability required for entry)", "PH - Rapid Re-Housing"),
         `Head of Household?` == "Yes",
         EnrollDate >= ymd("20180101")) %>%
  mutate(enroll_year = year(EnrollDate))

assessment_to_housed <- housing_program_data %>%
  inner_join(assessment_data, by = c("ClientID" = "CE_ClientID")) %>%
  filter(CE_EnrollDate <= EnrollDate,
         !is.na(DateOfMoveIn)) %>%
  ##  keep only most recent assessment for each enrollment
  arrange(desc(CE_EnrollDate)) %>%
  group_by(EnrollID) %>%
  slice(1L) %>%
  ungroup() %>%
  ##  keep only first enrollment for each assessment
  arrange(EnrollDate) %>%
  group_by(CE_EnrollID) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(days_to_housed = as.integer(ymd(DateOfMoveIn) - ymd(CE_EnrollDate)))

housing_program_data %>%
  group_by(enroll_year) %>%
  summarise(enrollments = n(), per_month = n()/12)

assessment_to_housed  %>%
  group_by(enroll_year)%>%
  summarize(enrollments = n(), av_num_days = round(mean(days_to_housed)), med_num_days = round(median(days_to_housed)))
  
