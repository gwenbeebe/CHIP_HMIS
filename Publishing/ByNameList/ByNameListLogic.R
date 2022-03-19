library(tidyverse)
library(lubridate)
library(readxl)

hud_service_data <- read_excel(paste0(getwd(), "\\Publishing\\SupplementalData.xlsx"), sheet = "Services") %>%
  left_join(read_excel(paste0(getwd(), "\\Publishing\\SupplementalData.xlsx"), sheet = "ServiceGroups"), 
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
}
  
zip_file <- file.choose()
zip_files <- unzip(zip_file, list = TRUE)

##  load in all files
export_data <- read_csv(unzip(zip_file, "Export.csv"),
                        col_types = "cicccciiiTTTccciii")
start_date <- as.Date(export_data$ExportStartDate)
end_date <- as.Date(export_data$ExportEndDate)

project_data <- read_csv(unzip(zip_file, "Project.csv"),
                         col_types = "ccccDDnnnnnnnnnTTcTc") 

organization_data <- read_csv(unzip(zip_file, "Organization.csv"),
                              col_types = "ccncTTcTn")

exit_data <- read_csv(unzip(zip_file, "Exit.csv"),
                      col_types = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc")

dv_data <- read_csv(unzip(zip_file, "HealthAndDV.csv"),
                    col_types = "cccDiiiiiiiDiiiiiTTcTc") %>%
  filter(CurrentlyFleeing == 1 &
           DataCollectionStage == 1) %>%
  select(EnrollmentID, CurrentlyFleeing) 

enrollment_data <- read_csv(unzip(zip_file, "Enrollment.csv"),
                            col_types = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc") %>%
  left_join(project_data %>%
              select(ProjectID, ProjectType, ProjectName, OrganizationID), by = "ProjectID") %>%
  left_join(organization_data %>%
              select(OrganizationID, OrganizationName), by = "OrganizationID") %>%
  left_join(exit_data %>%
              select(EnrollmentID, ExitDate, Destination), by = "EnrollmentID") %>%
  left_join(dv_data, by = "EnrollmentID") %>%
  dplyr::mutate(EntryDate = ymd(EntryDate),
         MoveInDate = ymd(MoveInDate),
         ExitDate = ymd(ExitDate)) %>%
  select(EnrollmentID, PersonalID, EntryDate, HouseholdID, RelationshipToHoH, ProjectID,
         LivingSituation, DateToStreetESSH, DisablingCondition, TimesHomelessPastThreeYears, 
         MonthsHomelessPastThreeYears, MoveInDate, CurrentlyFleeing, ProjectType,
         OrganizationName, ProjectName, ExitDate, Destination)

service_data <- read_csv(unzip(zip_file, "Services.csv"),
                         col_types = "cccDiicciciTTcTc") 

cls_data <- read_csv(unzip(zip_file, "CurrentLivingSituation.csv"),
                     col_types = "cccDiciiiiicTTcTc") 

client_data <- read_csv(unzip(zip_file, "Client.csv"),
                        col_types = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc") %>%
  select(PersonalID, DOB, VeteranStatus)


##  run event calculations

##  current living situation events
cls_events <- cls_data %>%
  filter(CurrentLivingSituation %in% c(homeless_situations, housed_situations)) %>%
  left_join(enrollment_data %>%
              select(EnrollmentID, ProjectName), by = "EnrollmentID") %>%
  dplyr::mutate(
    ClientStatus = case_when(
      CurrentLivingSituation %in% homeless_situations ~ "Homeless",
      CurrentLivingSituation %in% housed_situations ~ "Housed"),
    EventType = case_when(
      CurrentLivingSituation %in% homeless_situations ~ "Literally Homeless CLS",
      CurrentLivingSituation %in% housed_situations ~ "Housed CLS"),
    InformationSource = if_else(is.na(VerifiedBy), ProjectName, VerifiedBy)) %>%
  rename(EffectiveDate = InformationDate) %>%
  select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)

##  homeless enrollment events
homeless_enrollment_events <- enrollment_data %>%
  filter(LivingSituation %in% homeless_situations |
           CurrentlyFleeing == 1 |
           ProjectType %in% homeless_program_types) %>%
  dplyr::mutate(ClientStatus = "Homeless",
         EventType = "Literally Homeless Enrollment") %>%
  rename(EffectiveDate = EntryDate,
         InformationSource = ProjectName) %>%
  select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)

##  housing move-in date events
move_in_date_events <- enrollment_data %>%
  filter(MoveInDate >= start_date &
           ProjectType %in% housing_program_types) %>%
  dplyr::mutate(ClientStatus = "Housed",
         EventType = "Housing Move-In Date") %>%
  rename(EffectiveDate = MoveInDate,
         InformationSource = ProjectName) %>%
  select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)

##  service events
service_events <- service_data %>%
  filter(DateProvided >= start_date) %>%
  left_join(hud_service_data, by = c("RecordType", "TypeProvided")) %>%
  left_join(enrollment_data %>%
              select(EnrollmentID, ProjectName, ProjectType), by = "EnrollmentID") %>%
  filter(!is.na(ServiceType) | 
           ProjectType %in% homeless_program_types) %>%
  dplyr::mutate(
    EventType = case_when(
      ServiceType == "Housed" ~ "Rent or Deposit Service",
      ServiceType == "Homeless" ~ "Outreach Contact Service",
      TRUE ~ "Service From Homeless-Only Program"),
    ClientStatus = if_else(is.na(ServiceType), "Homeless", ServiceType)) %>%
  rename(EffectiveDate = DateProvided,
         InformationSource = ProjectName)%>%
  select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)

##  exits/residence events
other_enrollment_events <- enrollment_data %>%
  filter(
    (is.na(ExitDate) &
       (ProjectType %in% homeless_program_types |
          (ProjectType %in% housing_program_types &
             MoveInDate <= end_date))) |
      Destination %in% c(homeless_situations, housed_situations)) %>%
  dplyr::mutate(EffectiveDate = if_else(is.na(ExitDate), end_date, ExitDate),
         ClientStatus = case_when(
           Destination %in% homeless_situations |
             (ProjectType %in% c(1, 2, 8) &
                is.na(ExitDate)) ~ "Homeless",
           TRUE ~ "Housed"),
         EventType = case_when(
           Destination %in% homeless_situations ~ "Homeless Exit From Program",
           Destination %in% housed_situations ~ "Housed Exit From Program",
           TRUE ~ "Still Enrolled In Program")) %>%
  rename(InformationSource = ProjectName) %>%
  select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)

# Add Chronicity ----------------------------------------------------------
{
  # creating a small basic dataframe to work with
  smallEnrollment <- enrollment_data %>%
    select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, EntryDate,
           DateToStreetESSH, TimesHomelessPastThreeYears, ExitDate,
           MonthsHomelessPastThreeYears, DisablingCondition, ProjectType) %>%
    dplyr::mutate(ExitAdjust = if_else(is.na(ExitDate) |
                                  ExitDate > end_date, end_date, ExitDate)) %>%
    filter(ExitAdjust >= end_date - days(90))
  
  # getting only the independently-chronic clients. they're chronic right now
  # and because of *their own* homeless history
  singly_chronic <- smallEnrollment %>%
    dplyr::mutate(SinglyChronic =
             if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                         !is.na(DateToStreetESSH)) |
                        (
                          MonthsHomelessPastThreeYears %in% c(112, 113) &
                            TimesHomelessPastThreeYears == 4 &
                            !is.na(MonthsHomelessPastThreeYears) &
                            !is.na(TimesHomelessPastThreeYears)
                        )
             ) &
               DisablingCondition == 1 &
               !is.na(DisablingCondition), 1, 0))
  
  # pulling all EEs with the Chronic designation, marking all hh members of anyone
  # with a Chronic marker as also Chronic
  household_chronic <- singly_chronic %>%
    group_by(HouseholdID) %>%
    dplyr::mutate(
      ChronicHousehold = sum(SinglyChronic, na.rm = TRUE),
      ChronicStatus = case_when(
        ChronicHousehold > 0 ~ "Chronic",
        ChronicHousehold == 0 ~ "Not Chronic"
      )
    ) %>%
    ungroup() %>%
    select(-ChronicHousehold)
  
  # adds current days in ES or SH projects to days homeless prior to entry and if
  # it adds up to 365 or more, it marks the client as AgedIn
  agedIntoChronicity <- household_chronic %>%
    dplyr::mutate(
      DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                       ymd(EntryDate),
                                       units = "days"),
      DaysHomelessBeforeEntry = difftime(ymd(EntryDate),
                                         if_else(
                                           is.na(ymd(DateToStreetESSH)),
                                           ymd(EntryDate),
                                           ymd(DateToStreetESSH)
                                         ),
                                         units = "days"),
      ChronicStatus = if_else(
        ProjectType %in% c(1, 8) &
          ChronicStatus == "Not Chronic" &
          ymd(DateToStreetESSH) + days(365) > ymd(EntryDate) &
          !is.na(DateToStreetESSH) &
          DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
        "Aged In",
        ChronicStatus
      )
    ) %>%
    select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)
  
  # adds another ChronicStatus of "Nearly Chronic" which catches those hhs with
  # almost enough times and months to qualify as Chronic
  nearly_chronic <- agedIntoChronicity %>%
    dplyr::mutate(
      ChronicStatus = if_else(
        ChronicStatus == "Not Chronic" &
          ((
            ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
              !is.na(DateToStreetESSH)
          ) |
            (
              MonthsHomelessPastThreeYears %in% c(110:113) &
                TimesHomelessPastThreeYears%in% c(3, 4) &
                !is.na(MonthsHomelessPastThreeYears) &
                !is.na(TimesHomelessPastThreeYears)
            )
          ) &
          DisablingCondition == 1 &
          !is.na(DisablingCondition),
        "Nearly Chronic",
        ChronicStatus
      )
    ) %>%
    select(EnrollmentID, ChronicStatus)
  
  enrollment_data <- enrollment_data %>%
    left_join(nearly_chronic, by = "EnrollmentID")
  
  rm(smallEnrollment, singly_chronic, household_chronic, agedIntoChronicity, nearly_chronic)
}

all_events <- cls_events %>%
  union(homeless_enrollment_events) %>%
  union(move_in_date_events) %>%
  union(service_events) %>%
  union(other_enrollment_events) %>%
  filter(EffectiveDate >= start_date) %>%
  arrange(PersonalID, desc(EffectiveDate), desc(ClientStatus)) %>%
  distinct(PersonalID, EffectiveDate, .keep_all = TRUE)

client_statuses <- all_events %>%
  select(PersonalID, EffectiveDate, ClientStatus) %>%
  group_by(PersonalID) %>%
  dplyr::mutate(PriorDate = dplyr::lead(EffectiveDate),
                PriorStatus = dplyr::lead(ClientStatus),
                HomelessPrior90 = ClientStatus == "Homeless" &
                  PriorStatus == "Homeless" &
                  EffectiveDate - ddays(90) <= PriorDate &
                  !is.na(PriorStatus),
                IdentificationDate = suppressWarnings(max(case_when(
                  ClientStatus == "Homeless" &
                    !HomelessPrior90 ~ EffectiveDate), na.rm = TRUE)),
                HomelessEventInPeriod = suppressWarnings(max(
                  ClientStatus == "Homeless" &
                    EffectiveDate >= end_date - ddays(90), na.rm = TRUE)),
                HomelessEventBeforePeriod = suppressWarnings(max(
                  ClientStatus == "Homeless" &
                    EffectiveDate < end_date - ddays(90), na.rm = TRUE)),
                HousedBefore = suppressWarnings(max(
                  ClientStatus == "Homeless" &
                    PriorStatus == "Housed" &
                    EffectiveDate >= end_date - ddays(90), na.rm = TRUE))) %>%
  slice(1L) %>%
  filter(HomelessEventInPeriod == 1) %>%
  ungroup() %>%
  mutate(CurrentStatus = case_when(
    ClientStatus == "Housed" ~ "Housed",
    HomelessEventInPeriod &
      !HomelessEventBeforePeriod ~ "New to List",
    !HomelessEventInPeriod ~ "Inactive",
    HousedBefore == 1 ~ "Return From Housed",
    IdentificationDate >= end_date - ddays(90) ~ "Return From Inactive",
    TRUE ~ "Active"
  )) %>%
  select(PersonalID, CurrentStatus, IdentificationDate)

vet_statuses <- client_statuses %>%
  filter(CurrentStatus != "Housed") %>%
  inner_join(client_data %>%
           filter(VeteranStatus ==1) %>%
           select(PersonalID), by = "PersonalID") %>%
  mutate(PersonalID = as.integer(PersonalID))
