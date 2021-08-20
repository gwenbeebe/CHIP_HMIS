library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(stringr)


pit_data_location <- file.choose()
enrollment_data_location <- file.choose()


raw_pit_data <- read_excel(pit_data_location,
                       col_types = c("text", "text", "text", "text",
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text",
                                     "numeric", "text", "text", "text",
                                     "text", "text", "text", "text",
                                     "text", "text", "text", "logical",
                                     "text", "text", "text", "logical",
                                     "logical", "text", "text", "text",
                                     "text", "text", "date", "text",
                                     "text", "text", "text", "text",
                                     "text", "text", "text", "numeric",
                                     "logical", "logical", "numeric"))

raw_enrollment_data <- read_excel(enrollment_data_location, 
                              col_types = c("numeric", "numeric", "numeric", 
                                            "text", "text", "text", "text", 
                                            "text", "text", "text", "date", 
                                            "date", "date", "date", "text", 
                                            "numeric", "numeric", "text", "numeric", 
                                            "text", "text", "date", "numeric",
                                            "numeric", "text", "text", "date"))


pit_data <- raw_pit_data %>%
  `colnames<-`(paste(make.names(colnames(.), unique=TRUE), "PIT", sep = "_")) %>%
  select(id_PIT, DOB_PIT, Age.estimate_PIT,
         Age.Category_PIT, Ethnicity_PIT, Race_PIT, Gender_PIT, Initials_PIT,
         Birth.year_PIT, ClientTrack.ID_PIT) %>%
  mutate(join = 1,
         created_id = row_number())

enrollment_data <- raw_enrollment_data %>%
  `colnames<-`(paste(make.names(colnames(.), unique=TRUE), "E", sep = "_")) %>%
  select(ClientID_E, FirstName_E, LastName_E, Birthdate_E) %>%
  distinct() %>%
  mutate(Initials = case_when(
    !(str_detect(LastName_E, "Anonymous") | str_detect(FirstName_E, "Anonymous"))
    ~ paste0(
      substr(FirstName_E, 0, 1),
      substr(LastName_E, 0, 1)
  )), join = 1)

combined_data <- pit_data %>%
  left_join(enrollment_data, by = "join")

filtered_combined_data <- combined_data %>%
  filter(ClientTrack.ID_PIT == ClientID_E |
           (Initials_PIT == Initials &
              DOB_PIT == Birthdate_E) |
           is.na(ClientID_E)) %>%
  select(c(colnames(enrollment_data), created_id)) %>%
  full_join(pit_data, by = "created_id") %>%
  arrange(ClientID_E, created_id) %>%
  group_by(created_id) %>%
  slice(1L) %>%
  mutate(new_flag = if_else(is.na(ClientID_E), 1, 0),
         unmatchable_flag = if_else(
           is.na(ClientTrack.ID_PIT) &
             (is.na(Initials_PIT) |
             is.na(DOB_PIT)), 1, 0))

summary <- filtered_combined_data %>%
  group_by(unmatchable_flag, new_flag) %>%
  summarise(n = n())
