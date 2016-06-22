### Stock Analysis Functions

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

#Guppy Multiple Moving Average
GMMA <- function(x) {
  fastMA <- c(3,5,8,10,12,15)
  slowMA <- c(30,35,40,45,50,60)
  x <- sapply(c(fastMA,slowMA),
              function(xx)EMA(x,xx))
  return(x)   
}

################################################

addGuppy <- newTA(FUN=GMMA,
                  preFUN=Cl,
                  col=c(rep(3,6),
                        rep("#333333",6)),
                  legend="GMMA")

######################################################################

getSignals <- function(sym,rng,dta){
  mySymbol <- toupper(sym)
  dayRange <- as.numeric(rng)
  stockData <- dta
  names(stockData) <- c("Open","High","Low","Close","Volume","Adjusted")      
  
  stockData$BBand_pctB <- as.xts(BBands(Ad(stockData))$pctB)
  stockData$RSI <- as.xts(RSI(Ad(stockData)))
  stockData$PPO <- PPO(stockData)$ppoHist
  stockData$ATR <- ATR(stockData)$atr
  #stockData$EMA3 <- EMA(Ad(stockData), n=3)$Adjusted.EMA.3
  #stockData$EMA30 <- EMA(Ad(stockData), n=30)$Adjusted.EMA.30
  #stockData$EMADiff <- (stockData$EMA3 - stockData$EMA30) / stockData$EMA30
  stockData$mySignal <- (((stockData$RSI - 50)*3) + ((stockData$BBand_pctB - .5)*100)) /4 #+ (stockData$EMADiff * 100)
  stockData$mySignal <- (stockData$mySignal + 100) / 2
  stockData$stopPct <- ((stockData$ATR * 2) / Ad(stockData)) * 100
  return(stockData)
}

#tail(getSignals("SPY",30,SPY))

##### Chart  #################################################
getChart <- function(sym,rng,dta){
  mySymbol <- toupper(sym)
  dayRange <- as.numeric(rng)
  stockData <- as.xts(getSignals(mySymbol,dayRange,dta))
  chartSeries(last(stockData,dayRange),
              type="candlesticks",
              name=paste0(mySymbol, " - Past ", dayRange, " Days"),
              theme=chartTheme("white"),
              up.col="white",
              dn.col="black"     
  )
  #addTA(stockData$EMADiff, col="red")  #EMA diff turns out to just be the RSI...
  #addTA(stockData$mySignal, col="red")
  #addTA(stockData$BBand_pctB)
  
}

#########################################################################

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
scanner <- function(scanList) {
  if(exists("lastRow")==T) {
    rm(lastRow)
  }
  
  lastRow <- data.frame(matrix(ncol=13,nrow=1))
  #for testing scanList <- c("SPY","UUP")
  for(stock in scanList){
    currentStock <- NA
    currentStock <- eval(parse(text=stock))
    currentStock <- getSignals(stock,90,currentStock)
    lastRow <- as.data.frame(rbind(lastRow, c(stock, currentStock[nrow(currentStock),])))
    lastRow <- na.omit(lastRow)
  }
  names(lastRow) <- c("stock", "Open","High","Low","Close","Volume","Adjusted","BBand_pctB","RSI","PPO","ATR","mySignal","stopPct")
  
  lastRow$buysell <- ifelse(lastRow$BBand_pctB > 0 & lastRow$RSI > 60,"SELL",
                            ifelse(lastRow$BBand_pctB < 0 & lastRow$RSI < 40,"BUY","-"))
  #head(lastRow)
  #lastRow <- subset(lastRow, buysell != "-")
  
  return(lastRow)
}

# 
# scanned <- bbscan(stocklist)
# subset(scanned, signal!="-")
addStockData <- function(stockDF){
  
  #head(stockDF)
  #stockDF$Change_AdjYest <- Delt(stockDF$Adjusted, k=1)
  stockDF$Body_Pct <- OpCl(stockDF)
  stockDF$ClCl_Pct <- ClCl(stockDF)
  stockDF$GAP_Pct <- (Op(stockDF) - Lag(Cl(stockDF),k=1)) / Lag(Cl(stockDF),k=1)  # Gap from yesterday's close to today's open.
  stockDF$MaxProfit_1d_Pct <- (as.numeric(Next(Hi(stockDF),k=1))  - Cl(stockDF)) / Cl(stockDF)
  stockDF$AvgProfit_1d_Pct <- (((as.numeric(Next(Hi(stockDF),k=1))+as.numeric(Next(Lo(stockDF),k=1))) / 2 )  - Cl(stockDF)) / Cl(stockDF)
  
  stockDF <- fortify(stockDF)
  names(stockDF) <- c("Date","Op","Hi","Lo","Clo","Vol","Adj","Body_Pct","ClCl_Pct","GAP_Pct","MaxProfit_1d_Pct","AvgProfit_1d_Pct")
  stockDF$Body_Color[stockDF$Body_Pct >= 0] <- 'Green'
  stockDF$Body_Color[stockDF$Body_Pct < 0] <- 'Red'
  #stockDF$UpShadow[stockDF$Body_Color == 'Green'] <- stockDF$Hi - stockDF$Clo
  stockDF$UpShadow_Pct[stockDF$Body_Color=='Green'] <- (stockDF$Hi[stockDF$Body_Color=='Green'] - stockDF$Clo[stockDF$Body_Color=='Green']) / stockDF$Clo[stockDF$Body_Color=='Green']
  stockDF$UpShadow_Pct[stockDF$Body_Color=='Red'] <- (stockDF$Hi[stockDF$Body_Color=='Red'] - stockDF$Op[stockDF$Body_Color=='Red']) / stockDF$Op[stockDF$Body_Color=='Red']
  
  stockDF$DownShadow_Pct[stockDF$Body_Color=='Green'] <- (stockDF$Op[stockDF$Body_Color=='Green'] - stockDF$Lo[stockDF$Body_Color=='Green']) / stockDF$Op[stockDF$Body_Color=='Green']
  stockDF$DownShadow_Pct[stockDF$Body_Color=='Red'] <- (stockDF$Clo[stockDF$Body_Color=='Red'] - stockDF$Lo[stockDF$Body_Color=='Red']) / stockDF$Clo[stockDF$Body_Color=='Red']
  
  
  
  return(stockDF)
}
