library(shiny)
library(googlesheets)
library(RCurl)
require(lubridate)
library(ggplot2)

fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]

#########################
### Data Manipulation ###
#########################

## create time values ##

x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
df$run_time <- format(df$run_time, format='%H:%M:%OS')
df$week <- format(x, format='%W')
df$yday <- yday(x)
df$week[df$week == '00'] <- '01'

## All time sum of totals ##

df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
colnames(df1) <- c("person","miles")

## Sum of miles per person per day ##
df2 <- aggregate(df$miles, by=list(person=df$person, day=df$yday), FUN=sum)
colnames(df2) <- c("person","day","miles")

df3 <- aggregate(df$miles, by=list(yday=df$yday), FUN=sum)
colnames(df3) <- c("day","miles")

df4 <- aggregate(df$miles, by=list(person=df$person, week=df$week), FUN=sum)
colnames(df4) <- c("person", "week","miles")

## percent contribution to overall total ##

percent<- round(100*df1$miles/sum(df1$miles), 1)


Totals <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Accomplished Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)")) + theme_bw()


#####################
## Begin Shiny App ##
#####################

ui <- fluidPage(
  titlePanel("2017 Running Goal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plots","Select the Plot",c("geom_bar","Pie Chart"), selected = "Totals", selectize = TRUE),
      submitButton(text = "Refresh")
    ),
    mainPanel((""),
              plotOutput("plots")
      
    )
    ))

server <- function(input, output) {
  
  output$plots <- renderPlot(
    ggplot(
      data = {
        fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
        fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
        fileCSVDF <-  read.csv(textConnection(fileCSV))
        df <- fileCSVDF[,c(1:5)]
        x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
        df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
        df$run_time <- format(df$run_time, format='%H:%M:%OS')
        df$week <- format(x, format='%W')
        df$yday <- yday(x)
        df$week[df$week == '00'] <- '01'
        df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
        colnames(df1) <- c("person","miles")
        
      }, 
      aes(df1$person, df1$miles, fill=df1$person)) +
      input$plots(stat = "identity") +
      geom_text(aes(label=df1$miles), vjust=2, size=4) +
      labs(list(title = "Total Miles Accomplished Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)")) + theme_bw()
    )
}

shinyApp(ui = ui, server = server)


  

