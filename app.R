library(shiny)
library(googlesheets)
library(RCurl)
require(lubridate)
library(ggplot2)
library(gridExtra)


#####################
## Begin Shiny App ##
#####################
u <- shinyUI(fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  fluidRow(
    
    column(2,
           wellPanel(
             selectInput("plot","Select the Plot",c("Amy","Chad","Lauren","Naresh"), selected = "Amy", selectize = TRUE))       
    ),
    
    column(12,
           plotOutput("plotgraph")
    )
  )
))


############
## Server ##
############


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
      aes(week, miles)) + geom_line(color="slategray3") + geom_point(size=.75, color="slategray4") +
        geom_hline(yintercept = sum(df3$miles)/tail(df$week, n=1), color="indianred3", linetype = "dashed") +
        labs(list( x = "Week of Year Beginning Jan 01 2017", y = "Miles Logged"))  +
        annotate("text", x = 5, y = sum(df3$miles)/tail(df$week, n=1)+sum(df3$miles)/tail(df$week, n=1)*.05, label = {csvFileName <- paste(round(sum(df3$miles)/tail(df$week, n=1),2)," Average",sep="")}, size = 5.5, colour = "indianred3") +
        ggtitle({csvFileName <- paste("Weekly Distance Logged For ", input$plot, " \n Total Distance ", sum(df3$miles), sep="")}) + theme_minimal() + 
        theme(plot.title = element_text(hjust = .5))
    },
    {pt3 <- ggplot(data = {fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
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
    df2 <- aggregate(df$miles, by=list(df$person), 
                     FUN=sum, na.rm=TRUE)
    colnames(df2) <- c("person","miles")
    df3 <<- as.data.frame(df2)},aes(person, miles, fill=person, label=miles)) + geom_bar(stat = "identity") + labs(list( x = "Person", y = "Miles Logged", fill = "")) + scale_fill_brewer(palette="Set3") + ggtitle(paste("Miles Logged Per Person \n Total Miles completed = ", sum(df$miles), sep = "")) + theme_minimal() + geom_text(aes(label=miles), position=position_dodge(width=0.9), vjust=-0.25)
    pt3 + theme(legend.position = "NA") + theme(plot.title = element_text(hjust = .5)) 
    },
    {
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
        geom_text(size = 2.5, position = position_stack(vjust = 0.5)) + ggtitle("Where Miles Were Logged") + theme_minimal()
      pt2 + theme(legend.position = "top") + coord_polar("x") + scale_fill_brewer(palette="Set3") + theme(plot.title = element_text(hjust = .5))
    },ncol=3)
  )})

#########
## APP ##
#########
shinyApp(u,s)
