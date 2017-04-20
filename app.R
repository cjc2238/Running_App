library(shiny)
library(googlesheets)
library(RCurl)
require(lubridate)
library(ggplot2)


#####################
## Begin Shiny App ##
#####################

ui <- fluidPage(
  titlePanel("2017 Running Goal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot","Select the Plot",c("Amy","Chad","Lauren","Naresh"), selected = "Amy", selectize = TRUE),
      submitButton(text = "Submit")
    ),
    
    
    mainPanel((""),
              plotOutput("plot")
      
    )
    ))

server <- function(input, output) {
  
  output$plot <- renderPlot(
                            ggplot(
                                  data = 
                                    {fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
                                    fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
                                    fileCSVDF <-  read.csv(textConnection(fileCSV))
                                    df <- fileCSVDF[,c(1:5)]
                                    x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
                                    df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
                                    df$run_time <- format(df$run_time, format='%H:%M:%OS')
                                    df$week <- format(x, format='%W')
                                    df$yday <- yday(x)
                                    df$week[df$week == '00'] <- '01'
                                    df$person <- as.character(df$person)
                                    df$week <- as.numeric(df$week)
                                    df1 = subset(df, df$person %in% input$plot)
                                                                                },
                                  
  aes(week, miles, color=location, fill=location)) +
    stat_summary(fun.y = sum, geom = "bar", position = "identity",  inherit.aes = TRUE) + 
    labs(list(x = {csvFileName <- paste("Time in ","Weeks",sep="")}, y = "Miles"))  +
    ggtitle("Distance Logged") +
    theme(plot.title = element_text(hjust = 0.5))) 
    
}

shinyApp(ui = ui, server = server)


  

