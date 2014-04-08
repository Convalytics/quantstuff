### Stock Analysis

setwd("~/GitHub/quantstuff")

library(quantmod)

######################################################################################
PPO <- function(x) {
   x$ppo <- ((EMA(Ad(x),12) - EMA(Ad(x),26)) / EMA(Ad(x),26)) * 100
   x$signal <- EMA(x$ppo,9)
   x$ppoHist <- x$ppo - x$signal
   #return(x[,c("ppo","signal","ppoHist")])
   return(x)
}
####################################################################################


stockAnalysis <- function(a,b){
      mySymbol <- toupper(a)
      dayRange <- as.numeric(b)
      
      stockData <- eval(parse(text=mySymbol))
      names(stockData) <- c("Open","High","Low","Close","Volume","Adjusted")      
##### Add Signals #############################################


stockData$BBand_pctB <- as.xts(BBands(Ad(stockData))$pctB)
stockData$RSI <- as.xts(RSI(Ad(stockData)))
stockData$PPO <- PPO(stockData)$ppoHist
#stockData$Momentum <- Momentum(Ad(stockData))
stockData$EMADiff <- (Ad(stockData) - EMA(Ad(stockData),14))/Ad(stockData)
stockData$mySignal <- ((stockData$RSI) * ((stockData$BBand_pctB)))           #+ (stockData$PPO/5))
      
##### Chart  #################################################

      chartSeries(last(stockData,dayRange),
                  type="candlesticks",
                  name=paste0(mySymbol, " - Past ", dayRange, " Days"),
                  theme=chartTheme("white"),
                  up.col="white",
                  dn.col="black"     
      )
      #addTA(stockData$EMADiff, col="red")  #EMA diff turns out to just be the RSI...
      addTA(stockData$mySignal, col="red")

   }
   ######################################################################
#########################################################################
mySymbol <- "YHOO"
stockData <- eval(parse(text=mySymbol))

stockAnalysis(mySymbol,120)
addRSI()
#addRSI(10)
addTA(PPO(stockData)$ppoHist)
addTA(BBands(Ad(stockData))$pctB)
addEMA()
addMomentum()


#summary(BBands(Ad(stockData))$pctB)
