

require(RCurl)
fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]


# create time values
require(lubridate)
df$time_stamp <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
df$run_time2 <- format(df$run_time, format='%H:%M:%OS')
df$week <- format(df$time_stamp, format='%W')
df$yday <- yday(df$time_stamp)
df$year <- year(df$time_stamp)
df$week[df$week == '00'] <- '01'
df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'

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

p <- ggplot(df, aes(week, miles, colour=person, fill = person , label = miles)) + geom_bar(stat="identity")  + labs(list(title = "Total Miles Accomplished Per Week", x = "Week of Year", y = "Total Miles Accomplished")) + theme_light()

person_time_series <- p + scale_y_continuous(expand = c(0, .5)) + geom_hline(yintercept = 9.697, colour = "black", size = .8, linetype = "dotted") + annotate("text", x = 3, y = 10.3, label = "9.697 Miles", size = 3, colour = "black") + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(~ person)

print(person_time_series)

p <- ggplot(df3, aes(day, miles, label=miles))  + labs(list(title = "Total Miles Accomplished by Everyone (Per Day)", x = "Day of Year", y = "Total Miles Accomplished"))  

total_time_series <- p + geom_hline(yintercept = 5.526, colour = "black", size = .8, linetype = "dotted") + annotate("text", x = 21, y = 5.9, label = "Min Needed to meet year goal", size = 3, colour = "black") + theme_light() + theme(plot.title = element_text(hjust = 0.5)) + geom_point(color="black", size = 1) + geom_text(aes(label=miles),hjust=-.3, vjust=0, color = "black", size =3.5) + geom_smooth()

print(total_time_series)

totals <- ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Accomplished Per Person", x = "Person", y = "Total Distance Accomplished (miles)")) + theme_light() + theme(plot.title = element_text(hjust = 0.5))
print(totals)

p <- ggplot(df, aes(x=week, y=miles, col = location, fill = location)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90)) + theme_light() + facet_wrap(~ person)+ labs(list(title = "Total Miles Ran Per Week By Location and Person", x = "Week of Year", y = "Total Miles Accomplished"))

scatter <- p + theme(plot.title = element_text(hjust = 0.5)) 
print(scatter)

require(gridExtra)

one <- grid.arrange(person_time_series, totals, predicted_reg, total_reg, ncol=2)
grid.arrange(one, total_time_series, ncol=1)

grid.arrange(person_time_series, totals, total_time_series, ncol=1)

two <- grid.arrange(predicted_reg, totals, ncol=2)
grid.arrange(totals, person_time_series, ncol=2)

