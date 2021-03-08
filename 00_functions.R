library(tidyverse)
library(lubridate)
library(dplyr)

# call this function on dataframes with a client ID, a start date, and an end date
  # to combine all overlapping date ranges for each client into one row
combine_dates <- function(df.clientdates) {
  start_length = nrow(df.clientdates)
  end_length = 0
  temp_names <- c("ClientID", "start_date", "end_date")
  names_for_end <- colnames(df.clientdates)
  
  df_to_combine <- df.clientdates %>%
    setNames(temp_names)
  
  combined <- df_to_combine
  
  while (start_length != end_length)
  {
    start_length = nrow(combined)
    
    combined <- combined %>%
      left_join(combined %>%
                  setNames(paste(colnames(combined), "2", sep = "_"))
                , by = c("ClientID" = "ClientID_2")) %>%
      filter(start_date <= start_date_2 &
               end_date + ddays(1) >= start_date_2) %>%
      select(ClientID, start_date, end_date_2) %>%
      setNames(temp_names)
    
    combined <- combined  %>%
      group_by(ClientID, start_date) %>%
      mutate(end_date = max(end_date)) %>%
      ungroup() %>%
      distinct()%>%
      group_by(ClientID, end_date) %>%
      mutate(start_date = min(start_date)) %>%
      ungroup() %>%
      distinct()
    
    end_length = nrow(combined)
    
    # print(paste("Compressed", start_length, "rows into", end_length, "rows."))
  }
  
  combined %>%
    setNames(names_for_end)
}
