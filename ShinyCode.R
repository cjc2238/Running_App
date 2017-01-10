library(shiny)
library(googlesheets)
library(RCurl)

fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]

## Data Manipulation
df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
colnames(df1) <- c("person","miles")

## Plot Data
require(ggplot2)
p <- ggplot(df, aes(time_stamp, miles, colour=person)) + geom_point(size=2) + geom_line(aes(colour=person, group=person), size=1) + labs(list(title = "Time Series of Running Data", x = "Date", y = "Total Miles Ran"))
time_series <- p + scale_y_continuous(expand = c(0, .5))


totals <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Ran Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)"))


if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Number of observations", 0, 1000, 500),
    actionButton("goButton", "Go!"),
    plotOutput("distPlot")
  )
  
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      # Take a dependency on input$goButton. This will run once initially,
      # because the value changes from NULL to 0.
      input$goButton
      
      # Use isolate() to avoid dependency on input$obs
      dist <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Ran Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)"))
    })
  }
  
  shinyApp(ui, server)
  
}