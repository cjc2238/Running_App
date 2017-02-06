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
df$week[df$week == '00'] <- '01'

## Data Manipulation
# All time sum of totals

df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
colnames(df1) <- c("person","miles")

# Sum of miles per person per day
df2 <- aggregate(df$miles, by=list(person=df$person, day=df$yday), FUN=sum)
colnames(df2) <- c("person","day","miles")

df3 <- aggregate(df$miles, by=list(yday=df$yday), FUN=sum)
colnames(df3) <- c("day","miles")

df4 <- aggregate(df$miles, by=list(person=df$person, week=df$week), FUN=sum)
colnames(df4) <- c("person", "week","miles")


## Plot Data
require(ggplot2)
p <- ggplot(df4, aes(week, miles, colour=person)) + geom_point(size=2) + geom_line(aes(colour=person, group=person), size=1) + labs(list(title = "Time Series of Running Data", x = "Week of Year", y = "Total Miles Ran")) + theme_bw()

person_time_series <- p + scale_y_continuous(expand = c(0, .5)) + geom_hline(yintercept = 9.697, colour = "red", size = .8, linetype = "dotted") + annotate("text", x = .75, y = 10.75, label = "Min Needed to", size = 3, colour = "red") + annotate("text", x = .75, y = 10.25, label = "Meet Year Goal", size = 3, colour = "red") + theme(plot.title = element_text(hjust = 0.5))

print(person_time_series)

p <- ggplot(df3, aes(day, miles, label=miles)) + geom_point(size=2) + geom_line(size=1) + labs(list(title = "Time Series of Running Data", x = "Week of Year", y = "Total Miles Ran")) + geom_text(aes(label=miles),hjust=-0, vjust=-.5)

total_time_series <- p + geom_hline(yintercept = 5.526, colour = "red", size = .8, linetype = "dotted") + annotate("text", x = 0, y = 7.25, label = "Min Needed to", size = 3, colour = "red") + annotate("text", x = 0, y = 6.25, label = "Meet Year Goal", size = 3, colour = "red") + theme_bw() 

print(total_time_series)



totals <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Accomplished Per Person in 2017", x = "Person", y = "Total Distance Ran (miles)")) + theme_bw()
print(totals)

require(gridExtra)

grid.arrange(total_time_series, person_time_series, totals, ncol=1)
