# part 1 - loading and processing the data
setwd("~/JHDataScience/Reproducable_Research/RepData_PeerAssessment1")
list.files()
unzip(zipfile="activity.zip")
activity_df <- read.csv("activity.csv")

library(ggplot2)
library(sqldf)

# part 2 - steps by day
stepsByDay <- tapply(activity_df$steps, activity_df$date, sum, na.rm=T)
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
stepsByDayMean
stepsByDayMedian
qplot(stepsByDay, xlab='Total Steps per Day')

# part 3 - avg daily activity pattern
stepsByInterval <- sqldf("select interval, avg(steps) as avg_steps from activity_df group by interval")
head(stepsByInterval)
qplot(stepsByInterval$interval, stepsByInterval$avg_steps, data = stepsByInterval, geom="line",xlab ="interval", ylab = "steps", main = "Avg Steps Taken by Interval")
stepsByIntervalMax <- sqldf("select interval, avg(steps) as avg_steps from activity_df group by interval order by avg_steps desc limit 1")
stepsByIntervalMax

# part 4 - inputting missing values
activity_df_na <- sqldf("select count(*) as count_na from activity_df where steps is null")

activity_df[1,2]
# notes *1st num = row, 2nd num = column

# missing values
stepsByIntervalWithMax <- sqldf("select interval, max(steps) as avg_steps from activity_df group by interval")

populateMissingValues <- function(df){
    for(i in 1:nrow(df)){
        if(!is.null(df[i,1]) && is.na(df[i,1])){
            selected_interval <- df[i,3]
            selected_interval
            
            avg_steps <- stepsByIntervalWithMax[which(stepsByIntervalWithMax$interval == selected_interval),] 
            
            avg_steps <- avg_steps[1,2]
            avg_steps
            df[i,1] <- avg_steps
        }   
    }
    return (df)
}

activity_df_replace_na <- populateMissingValues(activity_df)
head(activity_df_replace_na)
head(df)
df <- activity_df
nrow(df)

stepsByIntervalReplaceNa <- sqldf("select interval, avg(steps) as avg_steps from activity_df_replace_na group by interval")
qplot(stepsByIntervalReplaceNa$interval, stepsByIntervalReplaceNa$avg_steps, data = stepsByIntervalReplaceNa, geom="line",xlab ="interval", ylab = "steps", main = "Avg Steps Taken by Interval")

stepsByDayRemoveNa <- tapply(activity_df_replace_na$steps, activity_df_replace_na$date, sum)
stepsByDayRemoveNaMean <- mean(stepsByDayRemoveNa)
stepsByDayRemoveNaMedian <- median(stepsByDayRemoveNa)
stepsByDayMean
stepsByDayMedian

# part 5 - weekend vs weekday
# weekends
stepsByIntervalDate <- sqldf("select date, interval, avg(steps) as avg_steps from activity_df_replace_na group by date,interval")
weekendSteps <- subset(stepsByIntervalDate, weekdays(as.Date(stepsByIntervalDate$date)) == "Saturday" | weekdays(as.Date(stepsByIntervalDate$date)) == "Sunday" )
stepsByIntervalWeekend <- sqldf("select interval, avg(avg_steps) as steps from weekendSteps group by interval")
plot1 <- qplot(stepsByIntervalWeekend$interval, stepsByIntervalWeekend$steps, data = stepsByIntervalWeekend, geom="line", xlab = "5-minute Interval", ylab ="Number of Steps")

# weekdays
weekendSteps <- subset(stepsByIntervalDate, weekdays(as.Date(stepsByIntervalDate$date)) != "Saturday" | weekdays(as.Date(stepsByIntervalDate$date)) != "Sunday" )
stepsByIntervalWeekday <- sqldf("select interval, avg(avg_steps) as steps from weekendSteps group by interval")
plot2 <- qplot(stepsByIntervalWeekday$interval, stepsByIntervalWeekday$steps, data = stepsByIntervalWeekday, geom="line", xlab = "5-minute Interval", ylab ="Number of Steps")

par(mfrow=c(1,1))  
with(stepsByIntervalDate, plot(avg_steps ~ interval, type="n", main="Weekday vs. Weekend Avg.", ylab = "steps", ylim=c(0,300)))  
with(stepsByIntervalWeekend, lines(steps ~ interval, type="l", col="chocolate"))  
with(stepsByIntervalWeekday, lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("chocolate", "16"), legend = c("weekday", "weekend"), seg.len=3)


    
    
