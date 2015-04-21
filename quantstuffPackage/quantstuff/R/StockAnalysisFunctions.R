### Stock Analysis Functions
# 
# setwd("~/GitHub/quantstuff")
# 
require(quantmod)

######################################################################################
#' PPO Function
#' 
PPO <- function(x) {
   x$ppo <- ((EMA(Ad(x),12) - EMA(Ad(x),26)) / EMA(Ad(x),26)) * 100
   x$signal <- EMA(x$ppo,9)
   x$ppoHist <- x$ppo - x$signal
   #return(x[,c("ppo","signal","ppoHist")])
   return(x)
}

####################################################################################

#Guppy Multiple Moving Average
#' Guppy Multiple Moving Average (GMMA)
#' 
GMMA <- function(x) {
   fastMA <- c(3,5,8,10,12,15)
   slowMA <- c(30,35,40,45,50,60)
   x <- sapply(c(fastMA,slowMA),
               function(xx)EMA(x,xx))
   return(x)   
}

################################################
#' Add Guppy
#' 
addGuppy <- newTA(FUN=GMMA,
                  preFUN=Cl,
                  col=c(rep(3,6),
                        rep("#333333",6)),
                  legend="GMMA")

######################################################################
#' Get Signals
#' 
getSignals <- function(sym,rng,dta){
   mySymbol <- toupper(sym)
   dayRange <- as.numeric(rng)
   stockData <- dta
   names(stockData) <- c("Open","High","Low","Close","Volume","Adjusted")      
   
   stockData$BBand_pctB <- as.xts(BBands(Ad(stockData))$pctB)
   stockData$RSI <- as.xts(RSI(Ad(stockData)))
   stockData$PPO <- PPO(stockData)$ppoHist
   stockData$ATR <- ATR(stockData)$atr
   stockData$EMA3 <- EMA(Ad(stockData), n=3)$Adjusted.EMA.3
   stockData$EMA30 <- EMA(Ad(stockData), n=30)$Adjusted.EMA.30
   stockData$EMADiff <- (stockData$EMA3 - stockData$EMA30) / stockData$EMA30
   stockData$mySignal <- (((stockData$RSI - 50)*3) + ((stockData$BBand_pctB - .5)*100)) /4 + (stockData$EMADiff * 100)
   stockData$stopPct <- ((stockData$ATR * 2) / Ad(stockData)) * 100
   return(stockData)
   
}

##### Chart  #################################################
#' Get Chart Function
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

#########################################################################
#' bbandscanner
bbandScanner <- function(a){
   
   mySymbol <- toupper(a)
   myStock <- eval(parse(text=mySymbol))
   
   bands <- BBands(Cl(myStock))$pctB
   bands$rsi <- RSI(Ad(myStock))
   bands$Close <- Ad(myStock)
   bands <- tail(bands, n=1)
   bands$signal <- NA
   bands$signal <- ifelse(bands$pctB > 1, "Sell",
                          ifelse(bands$pctB < 0,"Buy",
                                 "-"))
   return(bands)
}
########################################################

#### bbscan function ###############################################
#' bbscan function
bbscan <- function(stocklist){
   
   signalList <- as.data.frame(matrix(NA,nrow=0,ncol=5))
   names(signalList) <- c("stock","Close","rsi","pctB","signal")
   
   for(i in 1:length(stocklist)){
      testgrab <- bbandScanner(stocklist[i])
      signalList[i,"stock"]<-stocklist[i]
      signalList[i,"Close"]<-testgrab$Close
      signalList[i,"rsi"]<-  testgrab$rsi
      signalList[i,"pctB"]<-testgrab$pctB
      signalList[i,"signal"]<-testgrab$signal
      
   }
}
#############################################################################
#############################################################################
# 
# scanned <- bbscan(stocklist)
# subset(scanned, signal!="-")
