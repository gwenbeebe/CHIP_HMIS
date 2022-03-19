server <- function(input, output, session) {
  
  
  
  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    input$file # the file input data frame object that contains the file attributes
  })
  
  
  # Unzipping files on click of button and then rendering the result to dataframe
  observeEvent(input$unzip,
               output$zipped <- renderTable({
                 unzip(input$file$datapath, list = TRUE, exdir = getwd())
               })
               
  )
  
  output$affiliation <- renderTable({
    if(is.null(input$file)){return ()}
    read.csv(unz(input$file$datapath, unzip(input$file$datapath, list = TRUE, exdir = getwd())[1,1]))
    # the file input data frame object that contains the file attributes
  })
  
  
  csvs <- reactive({if(is.null(input$file)){return ()}
    unzip(input$file$datapath, files = c("Client.csv", "Enrollment.csv", "Exit.csv"))})
  
  
  
  reactive({if(is.null(input$file)){return ()}
           for (file in csvs()) {
             title <- str_extract(file, "[A-z]+" )
             csv_data <- read.csv(file)
             assign(title, csv_data)
             # rm(title, csv_data)
           }})
  
  output$test <- renderTable({
    if(is.null(input$file)){return ()}
    # read.csv(unz(input$file$datapath, unzip(input$file$datapath, list = TRUE, exdir = getwd())[2,1]))
    csv_data()
    # the file input data frame object that contains the file attributes
  })
  
  # new_enrollment <- reactive({
  #   Enrollment %>%
  #     left_join(Exit %>% 
  #                 select(EnrollmentID, 
  #                        ExitDate, 
  #                        Destination, 
  #                        OtherDestination),
  #               by = "EnrollmentID") %>%
  #     mutate(ExitAdjust = if_else(is.na(ExitDate) |
  #                                   ExitDate > today(),
  #                                 today(), ExitDate))
  # })
  
  # clients <- reactive({if(is.null(input$file)){return ()}
  #   read.csv(unz(input$file$datapath, unzip(input$file$datapath, list = TRUE, exdir = getwd())[2,1]))})
  
  output$label <- renderText({if(is.null(input$file)){return ()}
    colnames(data)[1]})
  
  
  
  
  
}