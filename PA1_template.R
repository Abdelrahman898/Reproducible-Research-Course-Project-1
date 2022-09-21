
# read our data
library(readr)
activity <- read_csv("activity.csv")
View(activity)
str(activity)
summary(activity)

library(ggplot2)
library(dplyr)
library(tidyr)
#Set language
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")

# Q1:
# What is mean total number of steps taken per day?

# we get sum of each steps per date and put it in variable x
x1<-activity %>% group_by(date) %>% summarise(sum(steps,na.rm=TRUE))
colnames(x1)<-c("Date","total number of steps per day")
str(x1) ; head(x1)
# get mean total num of steps per day
mean(x1$`total number of steps per day`)
# get median total num of steps per day
median(x1$`total number of steps per day`)

#create png with specific dimensions
png("total number of steps taken each day.png", width=480, height=480)
# plot histogram
hist(x1$`total number of steps per day`, main = "Histogram of Daily Steps", 
     col="brown", xlab="Steps", ylim = c(0,50))

# close the PNG device
dev.off()

# OR I CAN SUN RACH STEPS PER DAYS IF WEEKS HERE
activity<- mutate(activity,days=weekdays(date,abbreviate = T))
x2<-activity %>% group_by(days) %>% summarise(sum(steps,na.rm=TRUE))
colnames(x2)<-c("Days","total number of steps per day")
str(x2) ; head(x2)
# get mean total num of steps per day
mean(x2$`total number of steps per day`)
# get median total num of steps per day
median(x2$`total number of steps per day`)



# Q2:
# What is the average daily activity pattern?

# we get mean of each steps per 5 min interval and put it in variable x3
x3<-activity %>% group_by(interval) %>% summarise(meansteps=mean(steps,na.rm=TRUE))
str(x3) ; head(x3)

#create png with specific dimensions
png("average number of steps taken.png", width=480, height=480)
# Make a time series plot
plot(x3$interval,x3$meansteps,type = "l",xlab = "5 Minute Intervals",
     ylab = "Average Number of Steps",main = "Steps By Time Interval")

# close the PNG device
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
x3$interval[which.max(x3$meansteps)]
max(x3$meansteps)



# Q3:
# Imputing missing values


# filling in all of the missing values in the data set and put it in new table
new_activity <- activity
new_activity$steps[is.na(new_activity$steps)]=abs(rnorm(1:2304,mean = 1,sd = 0.2))
table(new_activity$steps)
View(new_activity)
str(new_activity)
summary(new_activity)
# OR USE replace_na TO REPLACE IT EITH ONE VALUE
# activity[,1]<-replace_na(activity$steps,2)

# Make a histogram of the total number of steps taken each day
x4<-new_activity %>% group_by(date) %>% summarise(totalsteps=sum(steps))
str(x4) ; head(x4)

#create png with specific dimensions
png("total number of steps taken each day after missing values are imputed.png", width=480, height=480)
# plot histogram
hist(x4$totalsteps, main = "Histogram of Daily Steps", 
     col="brown", xlab="Steps", ylim = c(0,50))

# close the PNG device
dev.off()


# the mean of the total number of steps taken per day
print(paste0("The mean is:",mean(x4$totalsteps)))
# the median of the total number of steps taken per day
print(paste0("The median is:",median(x4$totalsteps)))

# compare 
comparedata<-data.frame(mean=c(mean(x1$`total number of steps per day`),mean(x4$totalsteps)),
                        median=c(median(x1$`total number of steps per day`),median(x4$totalsteps)))
rownames(comparedata) <- c("Pre NA", "After NA")
print(comparedata)



# Q4:
# Are there differences in activity patterns between weekdays and weekends?


# Create a new factor variable in the data set with two levels “weekday” and “weekend”
# indicating whether a given date is a weekday or weekend day

activity$day <- if_else(weekdays(activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity$day<-as.factor(activity$day)
# let's put days name
activity<- mutate(activity,days=weekdays(date,abbreviate = T))
str(activity)

# Make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# subset our data 

# create weekday data set
weekday_activity <- subset(activity,day=='weekday',select = c(steps,interval,day))
View(weekday_activity)
table(weekday_activity$day)

x5<-weekday_activity %>% group_by(interval) %>% summarise(stepmean=mean(steps,na.rm=T))
x5$day <- "weekday"
str(x5) ; head(x5)

# create weekend data set
weekend_activity <- subset(activity,day=='weekend',select = c(steps,interval,day))
View(weekend_activity)
table(weekend_activity$day)

x6<-weekend_activity %>% group_by(interval) %>% summarise(stepmean=mean(steps,na.rm=T))
x6$day <- "weekend"
str(x6) ; head(x6)

# make them in one table
week_day_end <- rbind(x5,x6) 
str(week_day_end) ; head(week_day_end) ; summary(week_day_end)
View(week_day_end)


#create png with specific dimensions
png(" average number of steps taken per 5-minute interval across weekdays and weekends.png", width=480, height=480)

# plot
g <- ggplot(week_day_end,aes(interval,stepmean))+
      geom_line()+
      facet_grid(day~.)+
      xlab("5-Minute Interval")+
      ylab("Average Number Of Steps")+
      ggtitle("Time Series Plot - Weekday vs Weekend")

print(g)

# close the PNG device
dev.off()


