# Copyright (C) 2022 Gwen Beebe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

server <- function(input, output, session) {
  
  ##  load in all files
  export_data <- reactive({
    if(is.null(input$file)){return ()}
    withProgress(
      read_csv(unzip(input$file$datapath, "Export.csv"),
               col_types = "cicccciiiTTTccciii"))
  })
  
  start_date <- reactive({
    if(is.null(input$file)){return ()}
    as.Date(export_data()$ExportStartDate)
  })
  
  end_date <- reactive({
    if(is.null(input$file)){return ()}
    as.Date(export_data()$ExportEndDate)
  })
  
  project_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Project.csv"),
             col_types = "ccccDDnnnnnnnnnTTcTc") 
  })  
  
  organization_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Organization.csv"),
             col_types = "ccncTTcTn")
  })
  
  exit_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Exit.csv"),
             col_types = "cccDiciiiiiiiiiiiiiiiiiiiiiiiiiDiiiiiiTTcTc")
  })
  
  dv_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "HealthAndDV.csv"),
             col_types = "cccDiiiiiiiDiiiiiTTcTc") %>%
      filter(CurrentlyFleeing == 1 &
               DataCollectionStage == 1) %>%
      select(EnrollmentID, CurrentlyFleeing) 
  })
  
  enrollment_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Enrollment.csv"),
             col_types = "cccDciiiiiDiiiDDDiiiicccciiiDiiiiciiiiiiiiiiiiciiiiiiiiiiiiiiiiiiiiTTcTc")
  })
  
  
  
  service_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Services.csv"),
             col_types = "cccDiicciciTTcTc")
  })
  
  cls_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "CurrentLivingSituation.csv"),
             col_types = "cccDiciiiiicTTcTc")
  })
  
  client_data <- reactive({
    if(is.null(input$file)){return ()}
    read_csv(unzip(input$file$datapath, "Client.csv"),
             col_types = "ccccciciDiiiiiiiiiiiiiciiiiiiiiiiiiiTTcTc") %>%
      select(PersonalID, DOB, VeteranStatus)
  })
  
  #########################
  
  joined_enrollments <- reactive({
    if(is.null(input$file)){return ()}
    enrollment_data() %>%
      left_join(project_data() %>%
                  select(ProjectID, ProjectType, ProjectName, OrganizationID), by = "ProjectID") %>%
      left_join(organization_data() %>%
                  select(OrganizationID, OrganizationName), by = "OrganizationID") %>%
      left_join(exit_data() %>%
                  select(EnrollmentID, ExitDate, Destination), by = "EnrollmentID") %>%
      left_join(dv_data(), by = "EnrollmentID") %>%
      dplyr::mutate(EntryDate = ymd(EntryDate),
                    MoveInDate = ymd(MoveInDate),
                    ExitDate = ymd(ExitDate)) %>%
      select(EnrollmentID, PersonalID, EntryDate, HouseholdID, RelationshipToHoH, ProjectID,
             LivingSituation, DateToStreetESSH, DisablingCondition, TimesHomelessPastThreeYears, 
             MonthsHomelessPastThreeYears, MoveInDate, CurrentlyFleeing, ProjectType,
             OrganizationName, ProjectName, ExitDate, Destination)
  })
  
  
  ########################
  
  ##  run event calculations
  
  ##  current living situation events
  cls_events <- reactive({
    if(is.null(input$file)){return ()}
    cls_data() %>%
      filter(CurrentLivingSituation %in% c(homeless_situations, housed_situations)) %>%
      left_join(joined_enrollments() %>%
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
  })
  
  ##  homeless enrollment events
  homeless_enrollment_events <- reactive({
    if(is.null(input$file)){return ()}
    joined_enrollments() %>%
      filter(LivingSituation %in% homeless_situations |
               CurrentlyFleeing == 1 |
               ProjectType %in% homeless_program_types) %>%
      dplyr::mutate(ClientStatus = "Homeless",
                    EventType = "Literally Homeless Enrollment") %>%
      rename(EffectiveDate = EntryDate,
             InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  housing move-in date events
  move_in_date_events <- reactive({
    if(is.null(input$file)){return ()}
    joined_enrollments() %>%
      filter(MoveInDate >= start_date() &
               ProjectType %in% housing_program_types) %>%
      dplyr::mutate(ClientStatus = "Housed",
                    EventType = "Housing Move-In Date") %>%
      rename(EffectiveDate = MoveInDate,
             InformationSource = ProjectName) %>%
      select(PersonalID, EffectiveDate, ClientStatus, EventType, InformationSource)
  })
  
  ##  service events
  service_events <- reactive({
    if(is.null(input$file)){return ()}
    service_data() %>%
      filter(DateProvided >= start_date()) %>%
      left_join(hud_service_data, by = c("RecordType", "TypeProvided")) %>%
      left_join(joined_enrollments() %>%
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
  })
  
  ##  exits/residence events
  other_enrollment_events <- reactive({
    if(is.null(input$file)){return ()}
    joined_enrollments() %>%
      filter(
        (is.na(ExitDate) &
           (ProjectType %in% homeless_program_types |
              (ProjectType %in% housing_program_types &
                 MoveInDate <= end_date()))) |
          Destination %in% c(homeless_situations, housed_situations)) %>%
      dplyr::mutate(EffectiveDate = if_else(is.na(ExitDate), end_date(), ExitDate),
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
  })
  
  all_events <- reactive({
    if(is.null(input$file)){return ()}
    cls_events() %>%
      union(homeless_enrollment_events()) %>%
      union(move_in_date_events()) %>%
      union(service_events()) %>%
      union(other_enrollment_events()) %>%
      filter(EffectiveDate >= start_date()) %>%
      arrange(PersonalID, desc(EffectiveDate), desc(ClientStatus)) %>%
      distinct(PersonalID, EffectiveDate, .keep_all = TRUE) %>%
      mutate(before_inactive_date = 
               if_else(EffectiveDate < end_date() - ddays(input$days_to_inactive), 1, 0))
  })
  
  client_statuses <- reactive({
    if(is.null(input$file)){return ()}
    all_events() %>%
      select(PersonalID, EffectiveDate, ClientStatus, before_inactive_date) %>%
      group_by(PersonalID) %>%
      dplyr::mutate(PriorDate = dplyr::lead(EffectiveDate),
                    PriorStatus = dplyr::lead(ClientStatus),
                    HomelessPrior90 = ClientStatus == "Homeless" &
                      PriorStatus == "Homeless" &
                      EffectiveDate - ddays(input$days_to_inactive) <= PriorDate &
                      !is.na(PriorStatus),
                    IdentificationDate = suppressWarnings(max(case_when(
                      ClientStatus == "Homeless" &
                        !HomelessPrior90 ~ EffectiveDate), na.rm = TRUE)),
                    HomelessEventInPeriod = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        before_inactive_date == 0, na.rm = TRUE)),
                    HomelessEventBeforePeriod = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        before_inactive_date == 1, na.rm = TRUE)),
                    HousedBefore = suppressWarnings(max(
                      ClientStatus == "Homeless" &
                        PriorStatus == "Housed" &
                        before_inactive_date == 0, na.rm = TRUE))) %>%
      slice(1L) %>%
      ungroup() %>%
      mutate(CurrentStatus = case_when(
        ClientStatus == "Housed" ~ "Housed",
        HomelessEventInPeriod &
          !HomelessEventBeforePeriod ~ "New to List",
        !HomelessEventInPeriod ~ "Inactive",
        HousedBefore == 1 ~ "Return From Housed",
        IdentificationDate >= end_date() - ddays(input$days_to_inactive) ~ "Return From Inactive",
        TRUE ~ "Active"
      )) %>%
      filter(CurrentStatus %nin% c("Housed", "Inactive")) %>%
      select(PersonalID, CurrentStatus, IdentificationDate)
  })
  
  vet_statuses <- reactive({
    if(is.null(input$file)){return ()}
    client_statuses() %>%
      inner_join(client_data() %>%
                   filter(VeteranStatus == 1) %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  chronic_folks <- reactive({
    if(is.null(input$file)){return ()}
    enrollment_data() %>%
      inner_join(client_data() %>%
                   filter(DOB <= end_date - years(18)) %>%
                   select(PersonalID), by = "PersonalID") %>%
      arrange(desc(EntryDate)) %>%
      group_by(PersonalID) %>%
      slice(1:3) %>%
      ungroup() %>%
      filter(DisablingCondition == 1) %>%
      dplyr::mutate(SinglyChronic =
                      if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                                  !is.na(DateToStreetESSH)) |
                                 (
                                   MonthsHomelessPastThreeYears %in% c(112, 113) &
                                     TimesHomelessPastThreeYears == 4 &
                                     !is.na(MonthsHomelessPastThreeYears) &
                                     !is.na(TimesHomelessPastThreeYears)
                                 )
                      ), 1, 0)) %>%
      select(PersonalID, SinglyChronic) %>%
      group_by(PersonalID) %>%
      summarise(SinglyChronic = max(SinglyChronic)) %>%
      filter(SinglyChronic == 1)
  })
  
  chronic_statuses <- reactive({
    if(is.null(input$file)){return ()}
    client_statuses() %>%
      inner_join(chronic_folks() %>%
                   select(PersonalID), by = "PersonalID") %>%
      mutate(PersonalID = as.integer(PersonalID),
             IdentificationDate = ymd(IdentificationDate)) %>%
      arrange(PersonalID)
  })
  
  #########################
  output$effective_date <- renderUI({
    h4(
      if(is.null(input$file)){"No data uploaded"}
      else{
        paste(
          "Effective", format(end_date(), "%m-%d-%Y")
        )})
  })
  
  output$VBNL_active <- renderValueBox({
    valueBox(paste(
      if(is.null(input$file)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Active"))}, 
      "Veterans"),  
      "are actively homeless", icon = icon("campground"),
      color = "olive")})
  
  output$VBNL_newly <- renderValueBox({
    valueBox(paste(
      if(is.null(input$file)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "New to List"))}, 
      "Veterans"),  
      "are newly homeless", icon = icon("car-side"),
      color = "olive")})
  
  output$VBNL_return_h <- renderValueBox({
    valueBox(paste(
      if(is.null(input$file)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Return From Housed"))}, 
      "Veterans"),  
      "have returned from housing", icon = icon("house-damage"),
      color = "olive")})
  
  output$VBNL_return_i <- renderValueBox({
    valueBox(paste(
      if(is.null(input$file)){"---"}
      else{nrow(vet_statuses() %>%
               filter(CurrentStatus == "Return From Inactive"))}, 
      "Veterans"),  
      "have returned from inactive", icon = icon("undo"),
      color = "olive")})
  
  
  ##############################
  
  output$veteran_by_name_list <- renderDataTable({
    if(is.null(input$file)){return ()}
    DT::datatable(
      vet_statuses(),
      options = list(
        pageLength = 50, 
        initComplete = JS(
          "function(settings, json) {",
          "$('th').css({'text-align': 'center'});",
          "$('td').css({'text-align': 'center'});",
          "}")),
      selection = "single",
      rownames = FALSE) %>%
      formatStyle("PersonalID", `text-align` = 'center')
  })
  
  VBNL_events <- reactive({
    all_events()[which(vet_statuses()[[input$veteran_by_name_list_rows_selected,1]]==all_events()$PersonalID),]
  })
  
  observeEvent(input$veteran_by_name_list_rows_selected,{
    showModal(
      modalDialog(
        renderDataTable({
          DT::datatable(
            VBNL_events() %>%
              select(PersonalID, EffectiveDate, EventType, InformationSource, before_inactive_date),
            options = list(
              pageLength = 5,
              columnDefs = list(list(targets = 4, visible = FALSE))
              ),
            rownames = FALSE) %>% 
            formatStyle(
              c("PersonalID", "EffectiveDate", "EventType", "InformationSource"),
              'before_inactive_date',
              # target = 'row',
              backgroundColor = styleEqual(c(0, 1), c('White', 'WhiteSmoke'))
            )
        })
      ))
  })
  
  ###############
  
  output$chronic_by_name_list <- renderDataTable({
    if(is.null(input$file)){return ()}
    DT::datatable(
      chronic_statuses(),
      options = list(
        pageLength = 50, 
        initComplete = JS(
          "function(settings, json) {",
          "$('th').css({'text-align': 'center'});",
          "$('td').css({'text-align': 'center'});",
          "}")),
      selection = "single",
      rownames = FALSE) %>%
      formatStyle("PersonalID", `text-align` = 'center')
  })
  
  
}