
find_file <- file.choose()

filedf <- renderTable({
  if(is.null(find_file)){return ()}
  find_file # the file input data frame object that contains the file attributes
})


zipped <- unzip(find_file, list = TRUE, exdir = getwd())
zipped[1,1]


zipped_limit <- unzip(find_file, files = c("Client.csv", "Enrollment.csv"), exdir = getwd())


test <- read.csv(unz(find_file, zipped[1,1]))
