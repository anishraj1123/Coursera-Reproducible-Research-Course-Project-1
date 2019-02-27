'-------------------------------1.Read Data------------------------------------------'
setwd("C:/Users/samsung/Desktop/Coursera/Module 5/Week2")
fileurl <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "step_data.zip"
download.file(fileurl,destfile)
unzip(destfile)
activity <- read.csv("activity.csv",sep=",")

'---------------------------Processing the data to remove NA values---------------------------'
head(activity)
names(activity)
str(activity)
activity <- activity[which(!is.na(activity$steps)),]


'---------------------------2.Histogram of the total number of steps taken each day-------------'
stepsbyday <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)

png('plot1.png')
hist(stepsbyday,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")
dev.off()

'-----------------------------3.Mean and median number of steps taken each day------------------'
meanstepsbyday <- mean(stepsbyday)
medianstesbyday <- median(stepsbyday)

'-----------------------------4.Time Series plot of the average number of steps taken-----------'
stepsbyinterval <- aggregate(steps~interval,activity,mean,na.rm=TRUE)

png('plot2.png')
plot(steps~interval, data=stepsbyinterval, type="l")
dev.off()

'---------5.The 5-minute interval that, on average, contains the maximum number of steps--------'
maxstepsbyinterval <- stepsbyinterval[which.max(stepsbyinterval$steps),]


'---------------------------------6. Imputing missing data---------------------------------------'
sumofNA <- sum(is.na(activity$steps))

'To get the meanstpes for any interval'
getmeanstepsbyinterval <- function(interval){
    stepsbyinterval[stepsbyinterval$interval==interval,]$steps 
}

'Whichever row has NA values for steps column gets replaced by meansteps for the corresponding interval'
activityDatanoNA <- activity
for(i in 1:nrow(activity)){
    if(is.na(activity[i,]$steps)) {
        activityDatanoNA[i,]$steps <- getmeanstepsbyinterval(activityDatanoNA[i,]$interval)
    }
}

totalstepsperdaynoNA <- tapply(activityDatanoNA$steps,activityDatanoNA$date,sum,na.rm=TRUE)

'-----7.Histogram of the total number of steps taken each day after missing values are imputed-----'
png('plot3.png')
hist(totalstepsperdaynoNA,col = "green",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")
dev.off()

meanstepsbydaynoNA <- mean(totalstepsperdaynoNA)
medianstesbydaynoNA <- median(totalstepsperdaynoNA)


'---8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends--'
activityDatanoNA$date <- as.Date(strptime(activityDatanoNA$date,format="%Y-%m-%d"))
activityDatanoNA$day <- weekdays(activityDatanoNA$date)

for (i in 1:nrow(activityDatanoNA)){
    if(activityDatanoNA[i,]$day %in% c("Saturday","Sunday") ){
        activityDatanoNA[i,]$day <- "Weekend" 
    }
    else{
        activityDatanoNA[i,]$day <- "Weekday"
    }
}

stepsbyweekdayweekend <- aggregate(activityDatanoNA$steps~activityDatanoNA$interval+activityDatanoNA$day,data=activityDatanoNA,mean,na.rm=TRUE)
names(stepsbyweekdayweekend) <- c("Interval","Day","Steps")

library(lattice)
png('plot4.png')
xyplot(Steps~Interval|Day,data=stepsbyweekdayweekend, type="l", layout=c(1,2),
       xlab="Average")
dev.off()