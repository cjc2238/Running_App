## Pull Data from google Sheet

require(RCurl)
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
p + scale_y_continuous(expand = c(0, .5))


ggplot(data = df1, aes(person, miles, fill=person)) + geom_bar(stat = "identity") + geom_text(aes(label=df1$miles), vjust=2, size=4) + labs(list(title = "Total Miles Ran Per Person in 2017", x = "Person", y = "Total Miles Ran"))
