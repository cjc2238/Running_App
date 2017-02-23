require(RCurl)
fileUrl <- "https://docs.google.com/spreadsheets/d/1RTRABrYQhyyfj_Dr3bkmNSnxmM7jbXIMgrI8eOa7Avk/export?format=csv"
fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
fileCSVDF <-  read.csv(textConnection(fileCSV))
df <- fileCSVDF[,c(1:5)]

# create time values
require(lubridate)
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
df$yday <- yday(df$time_stamp)
df$year <- year(df$time_stamp)
df$week[df$week == '00'] <- '01'
df$weekday <- weekdays(df$time_stamp)
df$location[df$location == "Hell (treadmill)"] <- 'Treadmill'

# create data frame of only treadmill time

df <- df[ which(df$location=="Treadmill"), ]

df1 <- aggregate(df$miles, by=list(person=df$person), FUN=sum)
colnames(df1) <- c("person","miles")

# Sum of miles per person per day
df2 <- aggregate(df$miles, by=list(person=df$person, day=df$yday), FUN=sum)
colnames(df2) <- c("person","day","miles")

df3 <- aggregate(df$miles, by=list(yday=df$yday), FUN=sum)
colnames(df3) <- c("day","miles")

df4 <- aggregate(df$miles, by=list(person=df$person, week=df$week), FUN=sum)
colnames(df4) <- c("person", "week","miles")

# average run time per person per day
df5 <- aggregate(df$run_time_decimal, by=list(person=df$person, day=df$yday), FUN=sum)
colnames(df5) <- c("person","day","run_time_decimal")

# join aggregated daily run time and miles sum

df6 <- merge(df2, df5, by=c("person","day"))

# create time per mile 

df6$mile_time <- df6$miles/df6$run_time_decimal

# Create person specific df
df_chad <- df6[ which(df6$person=="Chad"), ]


# drop person column

df_chad <- df_chad[,-1]

# remove outliers

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

df_chad1 <- outlierKD(df_chad, miles)
no
df_chad1 <- outlierKD(df_chad, run_time_decimal)
no
# remove rows with na values

final_chad <- df_chad[complete.cases(df_chad),]

# Fit lm Model

model1 <- lm(mile_time ~ day, final_chad)

# get summary of the model

summary(model1)

# calculate correlation between day of year and miles ran
attach(final_chad)
cor(day, miles, method = "pearson")
detach(final_chad)
# create confidence interval

confint(model1, conf.level=0.95)

# check the regression diagnostic plots for this model

plot(model1)

# create regression plot function

ggplotRegression <- function (fit) {
  
require(ggplot2)
  
ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)), x = "Day of Year", y = "Time to Complete Run (MPH)")
}

# plot model
p <- ggplotRegression(model1)
p + theme_light()
