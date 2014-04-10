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


getSignals <- function(sym,rng,dta){
      mySymbol <- toupper(sym)
      dayRange <- as.numeric(rng)
      stockData <- dta
      names(stockData) <- c("Open","High","Low","Close","Volume","Adjusted")      
##### Add Signals #############################################

stockData$BBand_pctB <- as.xts(BBands(Ad(stockData))$pctB)
stockData$RSI <- as.xts(RSI(Ad(stockData)))
stockData$PPO <- PPO(stockData)$ppoHist
# 
stockData$ATR <- ATR(stockData)$atr
stockData$EMA3 <- EMA(Ad(stockData), n=3)$Adjusted.EMA.3
stockData$EMA30 <- EMA(Ad(stockData), n=30)$Adjusted.EMA.30
stockData$EMADiff <- (stockData$EMA3 - stockData$EMA30) / stockData$EMA30
stockData$mySignal <- (((stockData$RSI - 50)*3) + ((stockData$BBand_pctB - .5)*100)) /4 + (stockData$EMADiff * 100)
stockData$stopPct <- ((stockData$ATR * 2) / Ad(stockData)) * 100
return(stockData)

#####stockData$Momentum <- Momentum(Ad(stockData))

}


##### Chart  #################################################
getChart <- function(sym,rng,dta){
   mySymbol <- toupper(sym)
   dayRange <- as.numeric(rng)
   stockData <- as.xts(dta)
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
#myFaves <- c("TNA","SSO","UPRO","FAS","QLD","ERX","UWM","AGQ","DDM","UST","VIX")
#getSymbols(myFaves,src="yahoo", warnings = FALSE)

mySymbol <- "OIL"
days <- 120
myStock <- eval(parse(text=mySymbol))
#head(myStock)

thisStock <- getSignals(mySymbol, days, myStock)
tail(thisStock)

getChart(mySymbol,days,thisStock)
addEMA(n=3)
addEMA(n=30)
addBBands()



addRSI()
addTA(thisStock$BBand_pctB)
addTA(thisStock$PPO)
#addATR()
#addMomentum()
#tail((EMA(Cl(stockData), n=3) - EMA(Cl(stockData), n=30))/EMA(Cl(stockData), n=30), n=30)
