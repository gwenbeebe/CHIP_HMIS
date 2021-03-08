library(tidyverse)
library(lubridate)
library(dplyr)

source("00_functions.R")

lookback_start_date = floor_date(today(), "month") - years(3)
lookback_end_date = floor_date(today(), "month") - ddays(1)

lot_Enrollments <- Enrollments %>%
  mutate(LH_at_enrollment = if_else(ProgramType %in% c("Emergency shelter", "Street outreach", "Safe Haven") |
                                      # ## use these lines to keep only HUD-specified programs for measure 1b
                                      # (ProgramType %in% c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                                      #                     "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                                      #                     "PH - Rapid Re-Housing" ) & 
                                      ## use these lines to keep all non-prevention programs (program types not in SPM 1 only count for 3.917 data)
                                      (ProgramType != "Homelessness Prevention" & 
                                         (`Prior Living Situation(43)` %in% c("Place not meant for habitation", "Safe Haven",
                                                                              "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, or RHY-funded Host Home shelter") |
                                            (NightBeforeStay == "Yes" & !is.na(NightBeforeStay))
                                             )
                                         ), TRUE, FALSE),
         homeless_start_without_3.917 = as.Date(case_when(LH_at_enrollment ~ EnrollDate)),
         homeless_start_with_3.917 = as.Date(case_when(LH_at_enrollment ~ if_else(is.na(HomelessStartDate), EnrollDate, HomelessStartDate))),
         homeless_end_date = case_when(LH_at_enrollment ~
                                         case_when(ProgramType %in% c("Street outreach", "Coordinated Entry", "Day Shelter", "Services Only") ~ as.Date(EnrollDate),
                                                   ProgramType %in% c("PH - Permanent Supportive Housing (disability required for entry)",
                                                                      "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                                                                      "PH - Rapid Re-Housing" ) & !is.na(DateOfMoveIn) ~ ymd(DateOfMoveIn),
                                                   !is.na(ExitDate) ~ ymd(ExitDate),
                                                   TRUE ~ ymd(lookback_end_date))))%>%
  filter(homeless_end_date > lookback_start_date & homeless_start_without_3.917 < homeless_end_date) %>%
  select(EnrollID, ClientID, ProgramType, homeless_start_without_3.917, homeless_start_with_3.917, homeless_end_date)





all_dates <- as.data.frame(seq(min(lot_Enrollments$homeless_start_without_3.917), lookback_end_date, by="days")) %>%
  setNames("possible_day")



housed_dates <- Enrollments %>%
  filter(ClientID == 25148 &
           ProgramType %in% c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                            "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                            "PH - Rapid Re-Housing") &
           !is.na(DateOfMoveIn) &
           (is.na(ExitDate) | ExitDate >= lookback_start_date)) %>%
  mutate(housed_end_date = if_else(is.na(ExitDate), as.Date(lookback_end_date), as.Date(ExitDate))) %>%
  select(ClientID, DateOfMoveIn, housed_end_date) %>% 
  mutate(x = 1) %>%
  left_join(all_dates %>%
              mutate(x = 1),
            by = "x") %>%
  filter(DateOfMoveIn <= possible_day &
           housed_end_date > possible_day) %>%
  select(ClientID, possible_day) %>%
  distinct()
  
homeless_dates_without_3.917 <- lot_Enrollments %>%
  filter(ClientID == 25148) %>%
  mutate(x = 1) %>%
  left_join(all_dates %>%
              mutate(x = 1),
            by = "x") %>%
  filter(homeless_start_without_3.917 <= possible_day &
           homeless_end_date > possible_day) %>%
  select(ClientID, possible_day) %>%
  distinct() %>%
  anti_join(housed_dates) %>%
  mutate(end_date = possible_day + ddays(1)) %>%
  combine_dates()


df <- rbind(df, homeless_dates_without_3.917)


df <- data.frame(ClientID = str(n), start_date = as.Date(character()), end_date = as.Date(character()))
client <- data.frame(ClientID = str(n))
library(tictoc)

tic()
for (i in unique(lot_Enrollments$ClientID)) {
  # df <- rbind(df, data.frame(x = ClientID))#, y = toString(i)))
  housed_dates <- Enrollments %>%
    filter(ClientID == i &
             ProgramType %in% c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                                "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                                "PH - Rapid Re-Housing") &
             !is.na(DateOfMoveIn) &
             (is.na(ExitDate) | ExitDate >= lookback_start_date)) %>%
    mutate(housed_end_date = if_else(is.na(ExitDate), as.Date(lookback_end_date), as.Date(ExitDate))) %>%
    select(ClientID, DateOfMoveIn, housed_end_date) %>% 
    mutate(x = 1) %>%
    left_join(all_dates %>%
                mutate(x = 1),
              by = "x") %>%
    filter(DateOfMoveIn <= possible_day &
             housed_end_date > possible_day) %>%
    select(ClientID, possible_day) %>%
    distinct()
  
  homeless_dates_without_3.917 <- lot_Enrollments %>%
    filter(ClientID == i) %>%
    mutate(x = 1) %>%
    left_join(all_dates %>%
                mutate(x = 1),
              by = "x") %>%
    filter(homeless_start_without_3.917 <= possible_day &
             homeless_end_date > possible_day) %>%
    select(ClientID, possible_day) %>%
    distinct() %>%
    anti_join(housed_dates, by = c("ClientID", "possible_day")) %>%
    mutate(end_date = possible_day + ddays(1)) %>%
    combine_dates()
  
  df <- rbind(df, homeless_dates_without_3.917)
  
}
toc()





homelessness_without_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) #%>%
  combine_dates()

homelessness_with_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_with_3.917, homeless_end_date) %>%
  combine_dates()

housed <- Enrollments %>%
  filter(ProgramType %in% c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                            "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                            "PH - Rapid Re-Housing") &
           !is.na(DateOfMoveIn) &
           (is.na(ExitDate) | ExitDate >= lookback_start_date)) %>%
  mutate(housed_end_date = if_else(is.na(ExitDate), as.Date(lookback_end_date), as.Date(ExitDate))) %>%
  select(ClientID, DateOfMoveIn, housed_end_date) %>%
  combine_dates()


homelessness_without_3.917_new <- homelessness_without_3.917 %>%
  left_join(housed, by = "ClientID") %>%
  filter(!(DateOfMoveIn <= homeless_start_without_3.917
           & housed_end_date >= homeless_end_date) |
           is.na(DateOfMoveIn)) %>%
  mutate(homeless_start_without_3.917 = case_when(
    housed_end_date >= homeless_start_without_3.917 &
      housed_end_date <= homeless_end_date ~ housed_end_date, 
    TRUE ~ homeless_start_without_3.917)) %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  distinct() %>%
  filter(homeless_start_without_3.917 != homeless_end_date)


  
test <- lot_Enrollments %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  left_join(housed, by = "ClientID") %>%
  filter(!(DateOfMoveIn <= homeless_start_without_3.917
           & housed_end_date >= homeless_end_date) |
           is.na(DateOfMoveIn)) %>%
  mutate(homeless_start_without_3.917 = case_when(
    housed_end_date >= homeless_start_without_3.917 &
      housed_end_date <= homeless_end_date ~ housed_end_date, 
    TRUE ~ homeless_start_without_3.917)) %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  distinct() %>%
  filter(homeless_start_without_3.917 != homeless_end_date) %>%
  combine_dates()


remove_overlaps <- function(df.primary_dates, df.dates_to_remove) {
  temp_names <- c("ClientID", "start_date", "end_date")
  names_for_end <- colnames(df.primary_dates)
  
  df.dates_to_remove <- df.dates_to_remove %>%
    setNames(paste(temp_names, "remove", sep = "_"))
  
  df.primary_dates <- df.primary_dates %>%
    setNames(paste(temp_names, "primary", sep = "_")) %>%
    left_join(df.dates_to_remove, by = c("ClientID_primary" = "ClientID_remove")) %>%
    filter(!(start_date_remove <= start_date_primary
             & end_date_remove >= end_date_primary) |
             is.na(start_date_remove))
  
  # adjust_beginning <- 
    df.primary_dates %>%
    mutate(start_date_primary = case_when(
      end_date_remove >= start_date_primary &
        end_date_remove <= end_date_primary ~ as.Date(end_date_remove), 
      TRUE ~ as.Date(start_date_primary))) %>%
    select(ClientID_primary, start_date_primary, end_date_primary)
    
  adjust_ending <- df.primary_dates %>%
    mutate(end_date_primary = case_when(
      start_date_remove >= start_date_primary &
        start_date_remove <= end_date_primary ~ as.Date(start_date_remove), 
      TRUE ~ as.Date(end_date_primary))) %>%
    select(ClientID_primary, start_date_primary, end_date_primary)
  
  # adjust_beginning %>%
  #   full_join(adjust_ending, by = c("ClientID_primary" = "ClientID_primary", 
  #                                   "start_date_primary" = "start_date_primary", 
  #                                   "end_date_primary" = "end_date_primary")) %>%
  #   group_by(ClientID_primary, end_date_primary) %>%
  #   mutate(start_date_primary = max(start_date_primary)) %>%
  #   ungroup() %>%
  #   group_by(ClientID_primary, start_date_primary) %>%
  #   mutate(end_date_primary = min(end_date_primary)) %>%
  #   ungroup() %>%
  #   distinct() %>%
  #   filter(start_date_primary != end_date_primary) %>%
  #   setNames(names_for_end)
}

# temp <- combine_dates(homelessness_without_3.917) %>%
#   remove_overlaps(., housed) %>%
#   left_join(housed, by = c("ClientID_primary" = "ClientID")) %>%
#   filter(DateOfMoveIn < end_date_primary & housed_end_date > end_date_primary) 
#   
#   left_join(housed, by = "ClientID") %>%
#   filter((DateOfMoveIn >= homeless_start_without_3.917
#          & DateOfMoveIn < homeless_end_date) |
#            (housed_end_date >= homeless_start_without_3.917
#             & housed_end_date <= homeless_end_date))

  
  
  
# remove_overlaps_new <- function(df.primary_dates, df.dates_to_remove) {
df.primary_dates <- homelessness_without_3.917
df.dates_to_remove <- housed
  
  temp_names <- c("ClientID", "start_date", "end_date")
  names_for_end <- colnames(df.primary_dates)
  
  df.primary_dates <- df.primary_dates %>%
    setNames(paste(temp_names, "primary", sep = "_"))
  
  df.dates_to_remove <- df.dates_to_remove %>%
    setNames(paste(temp_names, "remove", sep = "_"))
  
  joined_table <- df.primary_dates %>%
    left_join(df.dates_to_remove, by = c("ClientID_primary" = "ClientID_remove")) 
  
  no_overlap <- joined_table %>%
    filter(is.na(start_date_remove) |
             start_date_primary >= end_date_remove |
             end_date_primary <= start_date_remove
             ) %>%
    select(ClientID_primary, start_date_primary, end_date_primary) %>%
    distinct() %>%
    setNames(temp_names)
  
  has_overlap <- df.primary_dates %>%
    anti_join(no_overlap, by = c("ClientID_primary" = "ClientID",
                                 "start_date_primary" = "start_date",
                                 "end_date_primary" = "end_date"))
  

  
  
  
homelessness_without_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  combine_dates() %>%
  mutate(id = row_number())


homelessness_with_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_with_3.917, homeless_end_date) #%>%
  # combine_dates()

housed <- Enrollments %>%
  filter(ProgramType %in% c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                            "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                            "PH - Rapid Re-Housing") &
           !is.na(DateOfMoveIn) &
           (is.na(ExitDate) | ExitDate >= lookback_start_date)) %>%
  mutate(housed_end_date = if_else(is.na(ExitDate), as.Date(lookback_end_date), as.Date(ExitDate))) %>%
  select(ClientID, DateOfMoveIn, housed_end_date) %>%
  combine_dates()
  
tic("test")
test <- homelessness_without_3.917 %>%
  left_join(housed, by = "ClientID") %>%
  mutate(DateOfMoveIn = case_when(!is.na(DateOfMoveIn) &
                                    DateOfMoveIn < homeless_end_date &
                                    DateOfMoveIn >= homeless_start_without_3.917 ~ DateOfMoveIn),
         housed_end_date = case_when(!is.na(housed_end_date) &
                                       housed_end_date < homeless_end_date &
                                       housed_end_date >= homeless_start_without_3.917 ~ housed_end_date)
         ) %>%
  group_by(id) %>%
  mutate(move_in_flag = max(if_else(!is.na(DateOfMoveIn), 1, 0)),
         homeless_end_date_new = if_else(move_in_flag == 1, as.Date(min(DateOfMoveIn, na.rm = TRUE)), as.Date(homeless_end_date)))
  # mutate(housed_end = min(DateOfMoveIn, na.rm = TRUE))
toc("test")


homelessness_without_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  combine_dates() %>%
  mutate(id = row_number()) %>%
  left_join(housed, by = "ClientID") %>%
  group_by(id) %>%
  mutate(homeless_start_without_3.917 = max(if_else(!is.na(housed_end_date) &
                                                      housed_end_date < homeless_end_date &
                                                      housed_end_date >= homeless_start_without_3.917, as.Date(housed_end_date), as.Date(homeless_start_without_3.917))),
         homeless_end_date = min(if_else(!is.na(DateOfMoveIn) &
                                  DateOfMoveIn < homeless_end_date &
                                  DateOfMoveIn >= homeless_start_without_3.917, as.Date(DateOfMoveIn), as.Date(homeless_end_date)))) %>%
  ungroup() %>%
  select(ClientID, homeless_start_without_3.917, homeless_end_date) %>%
  filter(homeless_start_without_3.917 <= homeless_end_date) %>%
  distinct()


homelessness_with_3.917 <- lot_Enrollments %>%
  select(ClientID, homeless_start_with_3.917, homeless_end_date) %>%
  combine_dates() %>%
  mutate(id = row_number()) %>%
  left_join(housed, by = "ClientID") %>%
  group_by(id) %>%
  mutate(homeless_start_with_3.917 = max(if_else(!is.na(housed_end_date) &
                                                      housed_end_date < homeless_end_date &
                                                      housed_end_date >= homeless_start_with_3.917, as.Date(housed_end_date), as.Date(homeless_start_with_3.917))),
         homeless_end_date = min(if_else(!is.na(DateOfMoveIn) &
                                           DateOfMoveIn < homeless_end_date &
                                           DateOfMoveIn >= homeless_start_with_3.917, as.Date(DateOfMoveIn), as.Date(homeless_end_date)))) %>%
  ungroup() %>%
  select(ClientID, homeless_start_with_3.917, homeless_end_date) %>%
  filter(homeless_start_with_3.917 < homeless_end_date) %>%
  distinct()

test <- homelessness_without_3.917 %>%
  mutate(diff = homeless_end_date - homeless_start_without_3.917)
