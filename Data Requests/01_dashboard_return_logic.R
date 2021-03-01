library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(xlsx)

# save_directory <- "C:/Users/GwenBeebe/CHIP/CES HMIS Team - Documents/"
save_directory <- "G:/CES-HMIS/"

##  PM = project management, E = equity, SP = system performance
file_to_generate <- readline(prompt="What dashboard is this for? (PM, E, SP, YHDP) ")

##  select enrollments file
file_name <- file.choose()

## extract dates from filename for naming final file
dates_for_file <- substr(basename(file_name), str_locate(basename(file_name), " ")[1,1], nchar(basename(file_name)) - 5)

##  read in file
Enrollments <- 
  read_excel(file_name)

##  Read in Programs data for PM dashboard
if (file_to_generate == "PM") {
  additional_program_data <- 
    read_excel(file.choose())
  Enrollments <- Enrollments %>%
    left_join(additional_program_data %>%
                select(ProgramID, ProgramName, ProgramType),
              by = "ProgramID")
}

##  Read in data quality report for system performance dashboard
if (file_to_generate == "SP") {
  data_quality <- 
    read_excel(file.choose())
  Enrollments <- Enrollments %>%
    left_join(data_quality %>%
                select(EnrollID, QuestionsToAnswer, QuestionsAnswered),
              by = "EnrollID")
}

{
  ##  set up definitions and initial dataframe, adjust as needed
  all_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                         "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                         "Emergency shelter", "RETIRED", "Services Only", "Homelessness Prevention", "Street outreach",
                         "PH - Rapid Re-Housing",  "Day Shelter", "Safe Haven", "Coordinated Entry")   
  
  return_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                            "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                            "Emergency shelter", "Street outreach", "PH - Rapid Re-Housing", "Safe Haven",  "Coordinated Entry")
  
  housing_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                             "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                             "PH - Rapid Re-Housing")
  
  ph_program_types <- c("PH - Permanent Supportive Housing (disability required for entry)", "PH – Housing Only", 
                        "PH – Housing with Services (no disability required for entry)", "PH - Rapid Re-Housing")
  
  permanent_destinations <- c("Rental by client, no ongoing housing subsidy",
                              "Rental by client, other ongoing housing subsidy",
                              "Rental by client in a public housing unit",
                              "Permanent housing (other than RRH) for formerly homeless persons",
                              "Rental by client, VASH housing Subsidy",
                              "Moved from one HOPWA funded project to HOPWA PH",
                              "Rental by client, with GPD TIP housing subsidy",
                              "Owned by client, no ongoing housing subsidy",
                              "Rental by client with RRH or equivalent subsidy",
                              "Rental by client, with HCV voucher (tenant or project based)",
                              "Owned by client, with ongoing housing subsidy",
                              "Staying or living with family, permanent tenure",
                              "Staying or living with friends, permanent tenure")
  
  ##  Update to include diversion
  include_diversion = TRUE
  if (include_diversion) {
    
    df_for_returns <- Enrollments %>%
      mutate(ProgramType = 
               if_else(ProgramName == "Diversion" 
                       | ProgramName == "IHN - Diversion"
                       | ProgramName == "YHDP - Diversion",
                       "Diversion", ProgramType)
      )
    
    return_program_types <- append(return_program_types, "Diversion")
    
  }
  
  df_for_returns <- df_for_returns %>%
    filter(ProgramType %in% return_program_types) %>%
    select(ClientID, EnrollID, EnrollDate, ExitDate, ProgramType, ExitDestination) %>%
    mutate(two_weeks_after_exit = if_else(!is.na(ExitDate), ExitDate + ddays(14), NULL))
  
  ##  find all exits from TH or PH programs
  housing_exits <- df_for_returns %>%
    filter(ProgramType %in% housing_program_types,
           !is.na(ExitDate)) %>%
    setNames(paste("H_Ex", colnames(df_for_returns), sep = "_"))
  
  ##  find all entries to PH programs
  ph_enrollments <- df_for_returns %>%
    filter(ProgramType %in% ph_program_types) %>%
    setNames(paste("PH_En", colnames(df_for_returns), sep = "_")) %>%
    select(-PH_En_two_weeks_after_exit)
  
  ##  identify all PH enrollments within 14 days of a TH or PH exit
  excluded_PH_entries <- ph_enrollments %>%
    left_join(housing_exits, by = c("PH_En_ClientID" = "H_Ex_ClientID")) %>%
    filter(PH_En_EnrollDate >= H_Ex_ExitDate &
             PH_En_EnrollDate <= H_Ex_two_weeks_after_exit) %>%
    select(PH_En_EnrollID) %>%
    distinct()
  
  ##  remove enrollments identified above from enrollments used to flag returns
  returning_entries <- df_for_returns %>%
    anti_join(excluded_PH_entries, by = c("EnrollID" = "PH_En_EnrollID")) %>%
    setNames(paste("R_En", colnames(df_for_returns), sep = "_"))
  
  ##  get all enrollments with permanent exits
  permanent_exits <- df_for_returns %>%
    filter(ExitDestination %in% permanent_destinations) %>%
    setNames(paste("PEx", colnames(df_for_returns), sep = "_")) %>%
    mutate(two_years_after_exit = PEx_ExitDate + dyears(2))
  
  ## create flag for all enrollments with a qualifying returning entry
  return_flags <- permanent_exits %>%
    left_join(returning_entries, by = c("PEx_ClientID" = "R_En_ClientID")) %>%
    group_by(PEx_EnrollID) %>%
    mutate(return_flag = 
             if_else(
               R_En_EnrollID != PEx_EnrollID &
               ((R_En_ProgramType %in% housing_program_types &
                  R_En_EnrollDate >= PEx_two_weeks_after_exit &
                  R_En_EnrollDate <= two_years_after_exit) |
                 (!R_En_ProgramType %in% housing_program_types &
                    R_En_EnrollDate >= PEx_ExitDate &
                    R_En_EnrollDate <= two_years_after_exit)),
               1, 0
             ),
           return_flag = if_else(is.na(max(return_flag)), 0, max(return_flag))) %>%
    ungroup() %>%
    select(PEx_EnrollID, return_flag) %>%
    distinct() %>%
    rename(EnrollID = PEx_EnrollID)
  
  flagged <- Enrollments %>%
    left_join(return_flags, by = "EnrollID") %>%
    mutate(return_flag = if_else(return_flag == 1, return_flag, NULL)) %>%
    rename("FLAG Returned" = "return_flag")
  
  if (file_to_generate == "PM") {
    flagged <- flagged %>%
      select(-ProgramName, -ProgramType)
  }
  
  ## add earliest return date for all enrollments with a qualifying returning entry
  return_dates <- permanent_exits %>%
    left_join(returning_entries, by = c("PEx_ClientID" = "R_En_ClientID")) %>%
    group_by(PEx_EnrollID) %>%
    mutate(return_flag = 
             if_else(
               R_En_EnrollID != PEx_EnrollID &
                 ((R_En_ProgramType %in% housing_program_types &
                     R_En_EnrollDate >= PEx_two_weeks_after_exit &
                     R_En_EnrollDate <= two_years_after_exit) |
                    (!R_En_ProgramType %in% housing_program_types &
                       R_En_EnrollDate >= PEx_ExitDate &
                       R_En_EnrollDate <= two_years_after_exit)),
               1, 0
             )) %>%
    filter(return_flag == 1) %>%
    arrange(PEx_EnrollID, R_En_EnrollDate) %>%
    slice(1L) %>%
    ungroup() %>%
    select(PEx_EnrollID, R_En_EnrollDate) %>%
    rename(EnrollID = PEx_EnrollID, ReturnDate = R_En_EnrollDate)
  
  dated <- Enrollments %>%
    left_join(return_dates, by = "EnrollID")
  
  if (file_to_generate == "PM") {
    # write_excel_csv(flagged, file = paste0(save_directory, "Dashboards/PM Dashboard/FlaggedEnrollments", dates_for_file, ".csv"), na = "")
    write.xlsx(flagged, file = paste0(save_directory, "Dashboards/PM Dashboard/FlaggedEnrollments", dates_for_file, ".xlsx"), showNA = FALSE)
  } else if (file_to_generate == "E") {
    write_excel_csv(flagged, file = paste0(save_directory, "Dashboards/Equity Dashboard/FlaggedEnrollments", dates_for_file, ".csv"), na = "")
  } else if (file_to_generate == "YHDP") {
    write_excel_csv(dated, file = paste0(save_directory, "Dashboards/YHDP Dashboard/FlaggedEnrollments", dates_for_file, ".csv"), na = "")
  } else {
    write_excel_csv(flagged, file = paste0(save_directory, "Dashboards/System Performance Dashboard/FlaggedEnrollments", dates_for_file, ".csv"), na = "")
  }
}
