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
      ))

##  select and load in .csv file with unsheltered client data
unsheltered_data <- file.choose(); unsheltered_data <- read.csv(unsheltered_data)

##  trim any initials of more than two characters down to the first and last 
##  character only
cleaned_unsheltered_data <- unsheltered_data %>%
  mutate(Initials = paste0(
    substr(Initials, 1, 1),
    substr(Initials, nchar(Initials), nchar(Initials))
    ))

##  get names of columns shared by both files
common_columns <- intersect(unlist(colnames(cleaned_sheltered_data)), 
                            unlist(colnames(cleaned_unsheltered_data)))

##  join files based on those common columns and de-duplicate
all_cleaned_data <- cleaned_sheltered_data %>%
  left_join(cleaned_unsheltered_data,
            by = common_columns) %>%
  arrange(desc(InShelterSince)) %>%
  distinct(Initials, Date.of.Birth, .keep_all = TRUE)



