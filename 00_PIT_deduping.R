library(tidyverse)
library(lubridate)

##  update this for each year!
PIT_date <- as.Date("2022-01-24")


################################################
################ Wheeler Import ################ 
################################################
##  this only has to be run by the person handling 
##  imports subsequent processing can use the 
##  generated and provided csv file
{
##  pick January csv export
wheeler_zip <- file.choose()
wheeler_files <- unzip(wheeler_zip, list = TRUE)


##  load in all files
project_data <- read.csv(wheeler_files[14,1])

enrollment_data <- read.csv(wheeler_files[5,1])
exit_data <- read.csv(wheeler_files[7,1])

client_data <- read.csv(wheeler_files[2,1]) %>%
  mutate(num_races = AmIndAKNative + Asian + BlackAfAmerican + NativeHIPacific + White,
         RaceBucket = case_when(
           num_races >= 2 ~ "Multiple Races",
           White == 1 ~ "White/Caucasian",
           NativeHIPacific == 1 ~ "Native Hawaiian/Pacific Islander",
           AmIndAKNative == 1 ~ "American Indian/Indigenous",
           Asian == 1 ~ "Asian/Asian American",
           BlackAfAmerican == 1 ~ "Black/African American/African",
           TRUE ~ ""),
         ## gender options are not complete for future, MissionTracker data only has binaries this year
         GenderBucket = case_when(
           Male == 1 ~ "Male",
           Female == 1 ~ "Female",
           TRUE ~ ""
         ),
         Initials = paste0(
           substr(FirstName, 1, 1),
           substr(LastName, 1, 1)),
         VeteranStatus = case_when(
           VeteranStatus == 0 ~ "N",
           VeteranStatus == 1 ~ "Y",
           TRUE ~ ""
         ))

disability_data <- read.csv(wheeler_files[3,1]) %>%
  filter(IndefiniteAndImpairs == 1) %>%
  group_by(PersonalID) %>%
  mutate(
    DisabilityResponse = if_else(
      DisabilityType == 10 & DisabilityResponse %in% c(1, 2, 3),
      DisabilityResponse, NULL),
    Alcohol.Abuse = max(
      if_else(DisabilityResponse %in% c(1, 3), "Y", "")),
    Drug.Abuse = max(
      if_else(DisabilityResponse %in% c(2, 3), "Y", "")),
    Physical.Disability = max(
      if_else(DisabilityType == 5, "Y", "")),
    Mental.Health = max(
      if_else(DisabilityType == 9, "Y", "")),
    HIV.AIDS = max(
      if_else(DisabilityType == 8, "Y", "")),
    Chronic.Health.Condition = max(
      if_else(DisabilityType == 7, "Y", ""))
  ) %>%
  select(PersonalID, Alcohol.Abuse, Drug.Abuse, Physical.Disability,
         Mental.Health, HIV.AIDS, Chronic.Health.Condition) %>%
  distinct()

# would pull in DV data if anyone was marked fleeing, but no one is
# dv_data <- read.csv(wheeler_files[10,1])



##  join up all files
PIT_enrollments <- enrollment_data %>%
  select(EnrollmentID, PersonalID, EntryDate, HouseholdID, RelationshipToHoH, ProjectID,
         TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears) %>%
  left_join(client_data %>%
              select(PersonalID, Initials, DOB, RaceBucket, GenderBucket, VeteranStatus, Ethnicity), by = "PersonalID") %>%
  left_join(disability_data, by = "PersonalID") %>%
  left_join(exit_data %>%
              select(EnrollmentID, ExitDate), by = "EnrollmentID") %>%
  left_join(project_data %>%
              select(ProjectID, ProjectName), by = "ProjectID") %>%
  mutate(ProjectID = if_else(ProjectID == 0, 4, as.double(ProjectID)),
         DOB = mdy(DOB, quiet = TRUE),
         SurveyID = "", SurveyDate = PIT_date, Interviewer = "MissionTracker", SurveyLocation = "MissionTracker",
         # no DV data recorded this year, revisit in 2023
         Currently.Fleeing.DV = "",
         FirstTimeHomeless = if_else(TimesHomelessPastThreeYears %in% c(2, 3, 4),
                                     "N", "Y"),
         MonthsHomelessLast3Years = case_when(
           MonthsHomelessPastThreeYears > 100 &
             MonthsHomelessPastThreeYears < 112 ~ "0-11",
           MonthsHomelessPastThreeYears >= 112 ~ "12+",
           TRUE ~ ""),
         Ethnicity = case_when(
           Ethnicity == 1 ~ "Y",
           Ethnicity == 0 ~ "N",
           TRUE ~ ""
         ),
         TimesHomelessLast3Years = if_else(
           TimesHomelessPastThreeYears %in% c(1, 2, 3, 4),
           as.character(TimesHomelessPastThreeYears), "")
  ) %>%
  filter(EntryDate <= PIT_date &
           (is.na(ExitDate) |
              ExitDate > PIT_date) &
           ProjectID != 5) %>%
  arrange(desc(EntryDate)) %>%
  distinct(PersonalID, .keep_all = TRUE) %>%
  rename(
    StayMonday = ProjectName, 
    ClientID = PersonalID,
    Date.of.Birth = DOB) %>%
  select(SurveyID, SurveyDate, Interviewer, SurveyLocation, StayMonday, ClientID,
         #        ChildrenWithYou, NumberofChildren, 
         TimesHomelessLast3Years, MonthsHomelessLast3Years,
         VeteranStatus, 
         Currently.Fleeing.DV, Alcohol.Abuse, Drug.Abuse, Physical.Disability,
         Mental.Health, HIV.AIDS, Chronic.Health.Condition,
         Ethnicity,
         RaceBucket, GenderBucket, Initials, Date.of.Birth, FirstTimeHomeless
  )

write.csv(PIT_enrollments, file = "Wheeler_PIT.csv", row.names = FALSE)
rm(list = ls(all.names = TRUE))
}


##  select and load in .csv file with sheltered client data
sheltered_data <- file.choose(); sheltered_data <- read.csv(sheltered_data)

##  fix date of birth formatting issue (caused by null DOBs)
cleaned_sheltered_data <- sheltered_data %>%
  mutate(Date.of.Birth = case_when(
    !is.na(Date.of.Birth) ~ as.Date(
      as.POSIXct(Date.of.Birth, format = "%m/%d/%Y %I:%M %p"))),
    InShelterSince = as.Date(
      as.POSIXct(InShelterSince, format = "%m/%d/%Y")
      ),
    FirstTimeHomeless = case_when(
      is.na(TimesHomelessLast3Years) | TimesHomelessLast3Years == 1 ~ "Y",
      TRUE ~ "N"),
    TimesHomelessLast3Years = as.character(TimesHomelessLast3Years),
    RaceBucket = case_when(
      RaceBucket == "White" ~ "White/Caucasian",
      RaceBucket == "Multi-Racial" ~ "Multiple Races",
      RaceBucket == "Native Hawaiian or Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
      RaceBucket =="American Indian, Alaska Native, or Indigenous" ~ "American Indian/Indigenous",
      RaceBucket == "Asian or Asian American" ~ "Asian/Asian American",
      RaceBucket == "Black, African American, or African" ~ "Black/African American/African",
      TRUE ~ ""),
    GenderBucket = case_when(
      Gender == "Male,Data not collected" ~ "Male",
      Gender == "Transgender,Male" ~ "Transgender",
      Gender == "A gender other than singularly female or male (e.g., non-binary, genderfluid, agender, culturally specific gender)" ~ 
        "A gender other than singularly female or male",
      TRUE ~ Gender
    ))

##  select and load in .csv file with unsheltered client data
unsheltered_data <- file.choose(); unsheltered_data <- read.csv(unsheltered_data, fileEncoding="UTF-8-BOM")

##  trim any initials of more than two characters down to the first and last 
##  character only
cleaned_unsheltered_data <- unsheltered_data %>%
  mutate(Initials = paste0(
    substr(Initials, 1, 1),
    substr(Initials, nchar(Initials), nchar(Initials))
    ),
    Date.of.Birth = case_when(
      !is.na(Date.of.Birth) ~ as.Date(
        as.POSIXct(Date.of.Birth, format = "%m/%d/%Y %I:%M %p"))),
    Mental.Health = case_when(
      Mental.Health == "Y" | PTSD == "Y" | BrainInjury.HeadTrauma == "Y" ~ "Y",
      TRUE ~ ""),
    FirstTimeHomeless = if_else(TimesHomelessLast3Years  %in% c("2", "3", "4", "4+"),
                                "N", FirstTimeHomeless),
    GenderBucket = if_else(GenderBucket == "male", "Male", GenderBucket))


##  get names of columns shared by both files
common_columns <- intersect(unlist(colnames(cleaned_sheltered_data)), 
                            unlist(colnames(cleaned_unsheltered_data)))

##  join HMIS shelter and unsheltered files based on those common columns and de-duplicate
cleaned_data <- cleaned_sheltered_data %>%
  full_join(cleaned_unsheltered_data,
            by = common_columns)

##  load in Wheeler csv data
wheeler_data <- file.choose(); wheeler_data <- read.csv(wheeler_data)

wheeler_data <- wheeler_data %>%
  mutate(TimesHomelessLast3Years = as.character(TimesHomelessLast3Years),
         Date.of.Birth = ymd(Date.of.Birth))

##  get names of columns shared by both files
common_columns <- intersect(unlist(colnames(cleaned_data)), 
                            unlist(colnames(wheeler_data)))

##  join PIT and Wheeler files based on those common columns and de-duplicate
cleaned_data <- cleaned_data %>%
  full_join(wheeler_data,
            by = common_columns) %>%
  group_by(Initials, Date.of.Birth, GenderBucket, RaceBucket) %>%
  mutate(Ethnicity = max(if_else(Ethnicity == "", "DK/R", Ethnicity)),
         VeteranStatus = max(if_else(VeteranStatus == "", "DK/R", VeteranStatus)),
         FirstTimeHomeless = max(if_else(FirstTimeHomeless == "", "R", FirstTimeHomeless))) %>%
  arrange(desc(InShelterSince)) %>%
  slice(1L) %>%
  ungroup()

write.csv(cleaned_data, "cleaned_pit_data.csv", row.names = FALSE)





















  







