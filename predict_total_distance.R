require(RCurl)
require(dplyr)
require(lubridate)

fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]

# create time values
df$time_stamp <- as.POSIXct(df$time_stamp, format='%m/%d/%Y %H:%M:%OS')
df$run_time <- as.POSIXct(df$run_time, format='%H:%M:%OS')
df$run_time <- format(df$run_time, format='%H:%M:%OS')
df$run_time_decimal <-  sapply(strsplit(df$run_time,":"),
                               function(x) {
                                 x <- as.numeric(x)
                                 x[1]+x[2]/60
                               }
)
df$week <- format(df$time_stamp, format='%W')
df$week <- as.numeric(df$week)
df$week[df$week == 0] <- 1
df$day <- yday(df$time_stamp)
df$year <- year(df$time_stamp)
df$week[df$week == '00'] <- '01'
df$weekday <- weekdays(df$time_stamp)
df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'

# Sum of miles per person per day
df1 <- aggregate(df$miles, by=list(day=df$day), FUN=sum)

# create year columns 

df1$year <- 2017

colnames(df1) <- c("day","miles","year")

# create cumulative sum column

df2 <- df1 %>% group_by(year) %>% mutate(cumsum = cumsum(miles))

# drop year columns 

df3 <- df2[,-c(2:3)]

# fit model

model1 <- lm(cumsum ~ day, df3)

# view model
summary(model1)

# create confidence interval

confint(model1, conf.level=0.95)

# plot model
plot(model1)

# create regression plot function

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5)), x = "Day of Year", y = "Total Distance Completed in Miles")
  }

# plot model
p <- ggplotRegression(model1)
total_reg <- p + theme_light() + theme(plot.title = element_text(hjust = 0.5))
total_reg

# create data frame to predict 

year_df <- data.frame(day = 53:365)

year_df$cumsum <- predict(model1, year_df)

# combine prediction to total dataframe

df4 <- rbind(df3, year_df)

predicted_total <- ggplot(year_df, aes(day, cumsum)) + geom_line() + geom_smooth(method = "lm", col = "blue") + labs(title = "Predicted Year End Distance", x = "Day of Year", y = "Total Distance Completed in Miles") + geom_hline(yintercept = 2017, colour = "red", size = .8, linetype = "dotted") + annotate("text", x = 70, y = 1875, label = "2017 Miles", size = 4, colour = "black")

predicted_reg <- predicted_total + theme_light() + theme(plot.title = element_text(hjust = 0.5)) + annotate("text", x = 360, y = 1250, label = "1455.6
Miles", size = 4, colour = "blue")

require(gridExtra)

grid.arrange(total_reg, predicted_reg, ncol=1)
