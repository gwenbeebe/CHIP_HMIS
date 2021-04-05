library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)

##  select enrollments file
file_name <- file.choose()

## extract dates from filename for naming final file
dates_for_file <- substr(basename(file_name), str_locate(basename(file_name), " ")[1,1], nchar(basename(file_name)) - 5)

##  get data from excel files
Enrollments <- read_excel(file_name)

Inventory <- read_excel(file.choose()) 



##  create full list of possible dates, including a column that has the first day of each month
all_dates <- as.data.frame(seq(min(Enrollments$ExitDate, na.rm = TRUE), max(Enrollments$EnrollDate), by="days")) %>%
  setNames("possible_day") %>%
  mutate(inventory_month = floor_date(possible_day, unit = "months"),
         join = 1)

##  create filtered enrollment table to only keep the information we need
utilization_enrollments <- Enrollments %>%
  filter(ProgramName %in% Inventory$ProgramName) %>%
  select(CaseID, ClientID, ProgramName, EnrollDate, ExitDate) %>%
  mutate(EnrollDate = as.Date(EnrollDate),
         impute_exit = if_else(!is.na(ExitDate), ExitDate, max(Enrollments$EnrollDate)),
         join = 1)

##  create filtered inventory table to only keep the information we need
utilization_inventory <- Inventory %>%
  select(ProgramName, `Start Date`, `End Date`, `Unit Inventory`, `Bed Inventory`) %>%
  filter(`Bed Inventory` > 0 |
           `Unit Inventory` > 0) %>%
  group_by(ProgramName) %>%
  mutate(`Bed Inventory` = if_else(is.na(`Bed Inventory`), 0, `Bed Inventory`),
         `Unit Inventory` = if_else(is.na(`Unit Inventory`), 0, `Unit Inventory`),
         join = 1) %>%
  ungroup()



## set up holding data frames
enrollment_holding <- data.frame(ProgramName=character(), 
                                 month=as.Date(character()),
                                 client_count=integer(), 
                                 hh_count=integer())

inventory_holding <- data.frame(ProgramName=character(), 
                                  month=as.Date(character()),
                                  bed_count=integer(), 
                                  unit_count=integer()) 



##  create tables with information on enrollments and inventories during each month
for (each_month in as.list(unique(all_dates$inventory_month))) {
  
  month_end <- ceiling_date(each_month + days(1), unit = "months") - days(1)
  
  days <- all_dates %>%
    filter(inventory_month == each_month)
  
  ##  create table with all enrollment counts for each month in the date list created above
  included_enrollments <- utilization_enrollments %>%
    filter(EnrollDate <= month_end &
             impute_exit > each_month) %>%
    inner_join(days, by = "join") %>%
    filter(EnrollDate <= possible_day &
             impute_exit > possible_day) %>%
    
    group_by(ProgramName, possible_day) %>%                 ##  get counts by day
    mutate(client_count = n_distinct(ClientID),
           caseID_count = n_distinct(CaseID)) %>%
    ungroup() %>%
    select(ProgramName, inventory_month, possible_day, client_count, caseID_count) %>%
    distinct() %>%
    
    group_by(ProgramName, inventory_month) %>%              ##  get counts by month
    mutate(client_count = sum(client_count),
           caseID_count = sum(caseID_count)) %>%
    ungroup() %>%
    select(ProgramName, inventory_month, client_count, caseID_count) %>%
    distinct()
  
  enrollment_holding <- rbind(enrollment_holding, included_enrollments)
  
  ##  create table with all inventory counts for each month in the date list created above
  active_inventories <- utilization_inventory %>%
    filter(`Start Date` <= month_end &
             `End Date` > each_month)%>%
    inner_join(days, by = "join") %>%
    filter(`Start Date` <= possible_day &
             `End Date` > possible_day) %>%
    
    group_by(ProgramName, possible_day) %>%                 ##  get counts by day
    mutate(bed_count = sum(`Bed Inventory`),
           unit_count = sum(`Unit Inventory`)) %>%
    ungroup() %>%
    select(ProgramName, inventory_month, possible_day, bed_count, unit_count) %>%
    distinct() %>%
    
    group_by(ProgramName, inventory_month) %>%              ##  get counts by month
    mutate(bed_count = sum(bed_count),
           unit_count = sum(unit_count)) %>%
    ungroup() %>%
    select(ProgramName, inventory_month, bed_count, unit_count) %>%
    distinct()
  
  inventory_holding <- rbind(inventory_holding, active_inventories)
  
}



##  join them together into one table that has the information for programs with inventories AND enrollments in a given month
utilization_summary <- enrollment_holding %>%
  inner_join(inventory_holding, by = c("ProgramName", "inventory_month"))



##  write table to csv for use in dashboards!
write_excel_csv(utilization_summary, file = paste0("UtilizationSummary", dates_for_file, ".csv"), na = "")