## Pull Data from google Sheet

require(RCurl)
fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]


# create time values
require(lubridate)
x <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
df$run_time <- format(df$run_time, format='%H:%M:%OS')
df$week <- format(x, format='%W')
df$yday <- yday(x)

## Data Manipulation
# All time sum of totals

df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
colnames(df1) <- c("person","miles")

# Sum of miles per person per day
df2 <- aggregate(df$miles, by=list(person=df$person, day=df$yday), FUN=sum)
colnames(df2) <- c("person","day","miles")

## Plot Data
require(ggplot2)
p <- ggplot(df2, aes(day, miles, colour=person)) + geom_point(size=2) + geom_line(aes(colour=person, group=person), size=1) + labs(list(title = "Time Series of Running Data", x = "Day of Year", y = "Total Miles Ran"))
time_series <- p + scale_y_continuous(expand = c(0, .5))
print(time_series)


totals <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Accomplished Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)"))
print(totals)

