library(tidyverse)

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
    RaceBucket = ca)

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
                                "N", FirstTimeHomeless))

##  get names of columns shared by both files
common_columns <- intersect(unlist(colnames(cleaned_sheltered_data)), 
                            unlist(colnames(cleaned_unsheltered_data)))

common_columns <- c("SurveyID", "SurveyDate", "Interviewer", "SurveyLocation", "StayMonday",
                    "ClientID", "ChildrenWithYou", "NumberofChildren",
                    "MonthsHomelessLast3Years", "Currently.Fleeing.DV", "Alcohol.Abuse", 
                    "Drug.Abuse", "Physical.Disability", "Mental.Health", "HIV.AIDS", 
                    "Chronic.Health.Condition", "Race", "Gender", "Initials", "Date.of.Birth")

##  join files based on those common columns and de-duplicate
all_cleaned_data <- cleaned_sheltered_data %>%
  full_join(cleaned_unsheltered_data,
            by = common_columns) %>%
  arrange(desc(InShelterSince)) #%>%
  distinct(Initials, Date.of.Birth, .keep_all = TRUE)


columns <- c("Initials", "Date.of.Birth", "Gender", "Race")

count_cleaned_data <- cleaned_sheltered_data %>%
  full_join(cleaned_unsheltered_data,
            by = common_columns) %>%
  arrange(desc(InShelterSince)) %>%
  group_by(Initials, Date.of.Birth, GenderBucket, RaceBucket) %>%
  summarize(n = n())

sets <- count_cleaned_data %>%
  filter(n > 1) %>%
  select(Initials, Date.of.Birth, GenderBucket, RaceBucket) %>%
  distinct()

for(pair in 1:nrow(sets)) {
  hold <- (sets[pair,]) %>%
    left_join(all_cleaned_data,
              by = c("Initials", "Date.of.Birth", "GenderBucket", "RaceBucket")) %>%
    select(Initials, Date.of.Birth, GenderBucket, RaceBucket, Ethnicity, VeteranStatus, FirstTimeHomeless) %>%
    group_by(Initials, Date.of.Birth, GenderBucket, RaceBucket) %>%
    mutate(NewEthnicity = max(if_else(Ethnicity == "Y", 1, 0)),
           NewVeteranStatus = max(if_else(VeteranStatus == "Y", 1, 0)),
           NewFirstTimeHomeless = max(if_else(FirstTimeHomeless == "Y", 1, 0))) %>%
    select(Initials, Date.of.Birth, GenderBucket, RaceBucket, 
           NewEthnicity, NewVeteranStatus, NewFirstTimeHomeless) %>%
    distinct()
}

cleaned_data <- cleaned_sheltered_data %>%
  full_join(cleaned_unsheltered_data,
            by = common_columns) %>%
  group_by(Initials, Date.of.Birth, GenderBucket, RaceBucket) %>%
  mutate(NewEthnicity = max(if_else(Ethnicity == "Y", 1, 0)),
         NewVeteranStatus = max(if_else(VeteranStatus == "Y", 1, 0)),
         NewFirstTimeHomeless = max(if_else(FirstTimeHomeless == "Y", 1, 0))) %>%
  arrange(desc(InShelterSince)) %>%
  slice(1L)




### to de-dupe, must match on initials and DOB
### do not de-dupe IF 3 of 5 elements DO NOT MATCH
### race bucket, ethnicity, gender bucket, veteran status, first time homeless

### when deduping, keep the row with the highest absolute number of values 
### do not count dk/r as values

### okay to collapse all mental health columns from paper surveys to match HMIS

### when someone appears on the paper surveys and also in the HMIS export, keep the HMIS export row


