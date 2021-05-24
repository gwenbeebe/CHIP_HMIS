library(shiny)
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
    # Upload zip files
    fileInput("file", "Upload Zip file", accept = ".zip"),
    # action button to unzip the file
    actionButton("unzip", "Unzip Files"),
    
    # to display the metadata of the zipped file
    tableOutput("filedf"),
    
    # to display the list of unzipped files
    tableOutput("zipped"),
    
    # to display affiliation
    tableOutput("affiliation")
    
)

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
    
    
}

shinyApp(ui, server)