#######################################################
# R functions for Knitr performance report  
#
# thertrader@gmail.com - Nov 2013
#######################################################
install.packages("MiKTeX")
library(knitr)
library(PerformanceAnalytics)
library(xtable)
library(xts)

performanceReport  <- function(inputPath=inputPath,
                               inputFile=inputFile,
                               keepColumns=keepColumns){
   
   data <- read.csv(paste(inputPath,inputFile,sep=""),sep=",")
   keepColumns <- keepColumns
   dataDaily <- data[,keepColumns]
   colnames(dataDaily) <- c("date","rtn")
   days <- as.Date(dataDaily[,"date"],"%m/%d/%Y") ##
   years <- as.numeric(sort(unique(substring(days,1,4))))
   months <- sort(unique(substring(days,1,7)))
   dailyRtn <- as.numeric(substring(dataDaily[,"rtn"],1,nchar(as.character(dataDaily[,"rtn"]))-1)) ##
   monthlyRtn <- aggregate(dailyRtn,by=list(substring(days,1,7)),sum)[,2]
   yearlyRtn <- aggregate(dailyRtn,by=list(substring(days,1,4)),sum)[,2]
   dailyDD <- as.vector(Drawdowns(dailyRtn/100))
   maxDD <- maxDrawdown(dailyRtn/100)
   currentYear <- as.numeric(substring(Sys.Date(),1,4))
   names(yearlyRtn) <- years
   names(monthlyRtn) <- months
   
   startYtd <- match(as.character(currentYear),substring(months,1,4))
   colorVectorMonth <- ifelse(monthlyRtn[startYtd:length(monthlyRtn)] > 0, 1, 2)
   colorVectorYear <- ifelse(yearlyRtn > 0, 1, 2)
   
   myxts <- xts(monthlyRtn/100,order.by=seq(as.Date("2000-01-30"), length=length(months), by="month")-2)
   colnames(myxts) <- "YTD"
   xtablePerfMonthly <- xtable(table.CalendarReturns(myxts,geometric=FALSE),
                               caption="Monthly Percentage Return (gross of fees)",
                               digits=1)
   
   print(xtablePerfMonthly,
         caption.placement = "top",
         include.rownames = TRUE,
         latex.environment="center",
         size="\\scriptsize")
   
   par(mfrow=c(3,2),cex=0.5,mex=0.3)
   plot(days,cumsum(dailyRtn),
        type="l",
        main="Equity Curve - Since Inception (%)",
        xlab="",
        ylab="")
   grid(col="dark grey")
   
   plot(days[match(as.character(currentYear),substring(days,1,4)):length(days)],
        cumsum(dailyRtn[match(as.character(currentYear),substring(days,1,4)):length(days)]),
        type="l",
        main="Equity Curve - YTD (%)",
        xlab="",
        ylab="")
   grid(col="dark grey")
   
   plot(days,dailyDD,
        type="l",
        xlab="",
        ylab="",
        main="maximum DrawDown - Since Inception (%)")
   grid(col="dark grey")
   
   plot(days[match(as.character(currentYear),substring(days,1,4)):length(days)],
        dailyDD[match(as.character(currentYear),substring(days,1,4)):length(days)],
        type="l",
        xlab="",
        ylab="",
        main="maximum DrawDown - YTD (%)")
   grid(col="dark grey")
   
   bpYear <- barplot(yearlyRtn,
                     border = NA,
                     col=colorVectorYear,
                     ylim=range(0,ceiling(max(yearlyRtn))+5),
                     main="Yearly Return - Since Inception (%)")
   text(bpYear,
        yearlyRtn,
        labels=as.character(round(yearlyRtn,2)),
        pos=3) 
   
   bpMonth <- barplot(monthlyRtn[startYtd:length(monthlyRtn)],
                      col=colorVectorMonth,
                      border = NA,
                      ylim=range(floor(min(monthlyRtn[startYtd:length(monthlyRtn)]))-1,ceiling(max(monthlyRtn[startYtd:length(monthlyRtn)]))+1), 
                      main="Monthly Return - YTD (%)")
   text(bpMonth,
        monthlyRtn[startYtd:length(monthlyRtn)],
        labels=as.character(round(monthlyRtn[startYtd:length(monthlyRtn)],2)),
        pos=3) 
   
   nbDays <- length(days)
   nbYears <- nbDays/252
   totalReturn <- sum(dailyRtn)
   annualizedReturn <- round(totalReturn/nbYears,2)
   annualizedVolatility <- round(sd(dailyRtn)*sqrt(252),2)
   sharpeRatio <- round(annualizedReturn/annualizedVolatility,2)
   
   maxDD <- 100*round(min(dailyDD),3)
   maxDDDate <- days[match(min(dailyDD),dailyDD)]  
   recoveryTime <- min(which(dailyDD[match(min(dailyDD),dailyDD):length(dailyDD)] == 0))
   
   monthlyHitRate <- 100*round(length(which(monthlyRtn > 0))/length(monthlyRtn),2)
   monthlyRtnAverage <- round(mean(monthlyRtn),2) 
   monthlyRtnPositive <- round(mean(monthlyRtn[which(monthlyRtn > 0)]),2) 
   monthlyRtnNegative <- round(mean(monthlyRtn[which(monthlyRtn < 0)]) ,2)
   worstMonth <- round(min(monthlyRtn),2)
   bestMonth <- round(max(monthlyRtn),2)
   
   dailyHitRate <- 100*round(length(which(dailyRtn > 0))/length(which(dailyRtn != 0)),2)
   dailyRtnAverage <- round(mean(dailyRtn),2) 
   dailyRtnPositive <- round(mean(dailyRtn[which(dailyRtn > 0)]) ,2)
   dailyRtnNegative <- round(mean(dailyRtn[which(dailyRtn < 0)]),2) 
   worstDay <- round(min(dailyRtn),2)
   bestDay <- round(max(dailyRtn),2)
   
   captionColumn1 <- c("Ann.Return","Ann.Volatility","Sharpe Ratio","","","")
   valueColumn1 <- c(annualizedReturn,annualizedVolatility,round(annualizedReturn/annualizedVolatility,2),"","","")
   captionColumn2 <- c("maxDD","maxDD Date","Time to Recover","","","")
   valueColumn2 <- c(maxDD,as.character(maxDDDate),paste(recoveryTime, "days",sep=" "),"","","")
   captionColumn3 <- c("Hit Rate","Mean Return","Mean > 0","Mean < 0","Worst","Best")
   valueColumn3 <- c(monthlyHitRate,monthlyRtnAverage,monthlyRtnPositive,monthlyRtnNegative,worstMonth,bestMonth)
   captionColumn4 <- c("Hit Rate","Mean Return","Mean > 0","Mean < 0","Worst","Best")
   valueColumn4 <- c(dailyHitRate,dailyRtnAverage,dailyRtnPositive,dailyRtnNegative,worstDay,bestDay)
   
   tradingStatistics <- cbind(captionColumn1,valueColumn1,captionColumn2,valueColumn2,captionColumn3,valueColumn3,captionColumn4,valueColumn4)
   colnames(tradingStatistics) <- c("Performance","(%)","Draw Down","(%)","Monthly","(%)","Daily","(%)")
   
   xtableResult <- xtable(tradingStatistics,
                          caption="Trading Statistics",
                          digits=2)
   
   print(xtableResult,
         caption.placement = "top",
         include.rownames = FALSE,
         size="\\scriptsize")  
}