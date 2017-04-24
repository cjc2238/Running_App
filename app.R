library(shiny)
library(googlesheets)
library(RCurl)
require(lubridate)
library(ggplot2)
library(gridExtra)


#####################
## Begin Shiny App ##
#####################

u <- fluidPage(
  titlePanel("2017 Running Goal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot","Select the Plot",c("Amy","Chad","Lauren","Naresh"), selected = "Amy",                  selectize = TRUE),
      submitButton(text = "Submit"), width = 2
    ),
    mainPanel("",
              column(12,plotOutput(outputId="plotgraph"))
    )
    ))

s <- shinyServer(function(input, output) 
{
  output$plotgraph = renderPlot(grid.arrange(
    {
    pt1 <- ggplot(data = {fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
    fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
    fileCSVDF <-  read.csv(textConnection(fileCSV))
    df <- fileCSVDF[,c(1:5)]
    x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
    df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
    df$run_time <- format(df$run_time, format='%H:%M:%OS')
    df$week <- format(x, format='%W')
    df$yday <- yday(x)
    df$week[df$week == '00'] <- '01'
    df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'
    df$person <- as.character(df$person)
    df$week <- as.numeric(df$week)
    df1 = subset(df, df$person %in% input$plot)
    df2 <- aggregate(df1$miles, by=list(df1$week), 
                     FUN=sum, na.rm=TRUE)
    colnames(df2) <- c("week","miles")
    df3 <<- as.data.frame(df2)},
    aes(week, miles)) + geom_line() + geom_point() +
        geom_hline(yintercept = sum(df3$miles)/tail(df$week, n=1), color="skyblue3", linetype = "dashed") +
        labs(list( x = "Week of Year Beginning Jan 01 2017", y = "Miles Logged"))  +
        annotate("text", x = 1.5, y = sum(df3$miles)/tail(df$week, n=1)+sum(df3$miles)/tail(df$week, n=1)*.05, label = {csvFileName <- paste(round(sum(df3$miles)/tail(df$week, n=1),2)," Average",sep="")}, size = 4, colour = "indianred3") +
        ggtitle({csvFileName <- paste("Weekly Distance Logged For ", input$plot, " \n Total Distance ", sum(df3$miles), sep="")}) + theme_linedraw() + 
        theme(plot.title = element_text(hjust = .5))
    },{
  pt2 <- ggplot(data = {fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
    fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
    fileCSVDF <-  read.csv(textConnection(fileCSV))
    df <- fileCSVDF[,c(1:5)]
    x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
    df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
    df$run_time <- format(df$run_time, format='%H:%M:%OS')
    df$week <- format(x, format='%W')
    df$yday <- yday(x)
    df$week[df$week == '00'] <- '01'
    df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'
    df$person <- as.character(df$person)
    df$week <- as.numeric(df$week)
    d <- as.data.frame(aggregate(df$miles, by=list(df$location, df$person), 
                                 FUN=sum, na.rm=TRUE))
    colnames(d) <- c("Location","person","miles")
    dd <- as.data.frame(d)
    },aes(x = person, y = miles, fill = Location, label = miles)) +
      geom_bar(stat = "identity") +
            labs(list( x = "Person", y = "Miles Logged", fill = "")) +
      geom_text(size = 2.5, position = position_stack(vjust = 0.5)) + 
      theme_linedraw()
  pt2 + theme(legend.position = "top") + coord_polar("x") + scale_fill_brewer(palette="Set3")
  },
  {ggplot(data = {fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
  fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
  fileCSVDF <-  read.csv(textConnection(fileCSV))
  df <- fileCSVDF[,c(1:5)]
  x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
  df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
  df$run_time <- format(df$run_time, format='%H:%M:%OS')
  df$week <- format(x, format='%W')
  df$yday <- yday(x)
  df$week[df$week == '00'] <- '01'
  df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'
  df$person <- as.character(df$person)
  df$week <- as.numeric(df$week)
  df1 <- as.data.frame(df)},aes(person, miles, fill=person, label=miles)) + geom_bar(stat = "identity") + labs(list( x = "Person", y = "Miles Logged", fill = "")) + scale_fill_brewer(palette="Set3")
  },ncol=3)
  )})

shinyApp(u,s)
