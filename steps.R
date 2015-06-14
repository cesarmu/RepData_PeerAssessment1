steps <- function(){
    library(sqldf)
    library(lattice)
    
    stepsData <- read.csv("activity.csv")
    
    stepsDataNoNA <- na.omit(stepsData)
    
    sumSteps <- aggregate(stepsDataNoNA[,1], list(minute=stepsDataNoNA$date), sum)

    png("figure/plot1.png",width=480,height=480)
    
    hist(sumSteps$x, breaks=seq(0,25000,by=5000), freq=TRUE, 
         xlab="day", main="Total steps by day", col="Red")
    #head(stepsData)
    dev.off()
    
    print(mean(sumSteps$x))
    print(median(sumSteps$x))    
    
    
    averageSteps <- aggregate(stepsDataNoNA[,1], list(interval=stepsDataNoNA$interval), mean)
    
    png("figure/plot2.png",width=480,height=480)
    plot(averageSteps$interval, averageSteps$x, type="l",
         xlab="Five min interval", ylab="Steps", 
         main="The average number of steps taken")
    dev.off()
    
    max_interval <- averageSteps[which.max(averageSteps$x),]
    print(max_interval)
    
    
    #Imputing missing values
    #Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    
    NaValuesData <- sqldf('select s.* from "stepsData" as s
                          where s.steps is null
                          order by s.date, s.interval')
    #Número de registros con NA
    print(nrow(NaValuesData))
    
    fillMeanValuesData <- sqldf('select s.*, a.x
                                 from "averageSteps" as a
                                join "stepsData" as s
                                on a.interval = s.interval
                                order by s.date, s.interval')
    
    #Llena los renglones de NA con la media del intervalo correpondiente
    fillMeanValuesData$steps[is.na(fillMeanValuesData$steps)] <- fillMeanValuesData$x[is.na(fillMeanValuesData$steps)]
    
    #tail(fillMeanValuesData, 500)
    
    sumMeanValuesData <- aggregate(fillMeanValuesData[,1], list(y=fillMeanValuesData$date), sum)
    
    png("figure/plot3.png",width=480,height=480)
    hist(sumMeanValuesData$x, breaks=seq(0,25000,by=5000), 
         freq=TRUE, xlab="day", main="Total steps by day", col="Red")
    #head(stepsData)
    dev.off()
    
    print(mean(sumMeanValuesData$x))
    print(median(sumMeanValuesData$x))     
    
    #fillMeanValuesData$date <- as.POSIXlt(fillMeanValuesData$date,format="%Y-%m-%d")
    
    #fillMeanValuesData$weekDay <- NULL
    #fillMeanValuesData$weekDay <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
    #                             "Friday", "Saturday")[as.POSIXlt((fillMeanValuesData$date)$wday + 1
    #                                      ,format="%Y-%m-%d")]
    
    fillMeanValuesData$day <- c("weekend", "weekday", "weekday", "weekday", "weekday", 
                                    "weekday", "weekend")[as.POSIXlt(fillMeanValuesData$date)$wday + 1]
    
    
    stepsByDay <- aggregate(steps ~ interval + day, data = fillMeanValuesData, mean)
    
    
    myplot <- xyplot(steps ~ interval | day, data = stepsByDay, type="l",
                xlab="Interval", ylab="Number of steps", layout=c(1,2))
    
    #setwd("~/")
    trellis.device(device="png", filename="figure/plot4.png")
    print(myplot)
    dev.off()
    
    #head(stepsByDay)
    

}