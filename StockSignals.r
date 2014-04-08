### Stock Signaler

## quantstrat quant strategy model framework
## blotter : trading system development
## TTR : technical trading rules



library(quantmod)
#library(PerformanceAnalytics)
setwd("~/GitHub/quantstuff")

#############################################################################

chartStock <- function(a,b){
   mySymbol <- toupper(a)
   dayRange <- as.numeric(b)
   
   # If the quote object doesn't exist. Get it from Yahoo.
   # If it already exists, this code will be skipped.
   if(exists(mySymbol)==F) {
      getSymbols(mySymbol,src="yahoo")
   }
   
   #    todayQuote <- getQuote(mySymbol, what=yahooQuote.EOD)
   myStock <- eval(parse(text=mySymbol))
   
   chartSeries(last(myStock,dayRange),
               type="candlesticks",
               name=paste0(mySymbol, " - Past ", dayRange, " Days"),
               theme=chartTheme("white"),
               up.col="white",
               dn.col="black"     
   )
   addTA(PPO(myStock)$ppoHist, col="red",type="h")
   #    addTA(BBands(Cl(myStock))$pctB)
   #    addTA(BBands(Cl(myStock)),on=-1)
   #    addGuppy()
   #addBBands()
   #    addMACD()
   #####  todayQuote
}
######################################################################
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


# addPPO <- newTA(FUN=PPO,
#                 preFun=Ad)
# 
# PPO(XLK)
# addPPO()
# 
# addMACD()
###################################################

#Percentage Price Oscillator (PPO): {(12-day EMA - 26-day EMA)/26-day EMA} x 100
#Signal Line: 9-day EMA of PPO
#PPO Histogram: PPO - Signal Line

PPO <- function(x) {
   x$ppo <- ((EMA(Ad(x),12) - EMA(Ad(x),26)) / EMA(Ad(x),26)) * 100
   x$signal <- EMA(x$ppo,9)
   x$ppoHist <- x$ppo - x$signal
   #return(x[,c("ppo","signal","ppoHist")])
   return(x)
}
# test <- PPO(AUY)
# ppo <- ((EMA(Ad(AUY),12) - EMA(Ad(AUY),26)) / EMA(Ad(AUY),26)) * 100
# signal <- EMA(ppo,9)
# ppoHist <- ppo - signal
# tail(ppoHist)
# tail(test)
######################################################################
######################################################################
### End Functions
######################################################################
######################################################################




### Goog split causing issues???
#stocklist<-c("AUY","XLK","AAPL","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB")
#              "MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM",
#              "CTSH","ADBE","GLW","AMAT","MU","TEL","INTU","CTL","SNDK","WDC","STX","ADI","BRCM",
#              "FIX","APH","XLNX","FISV","NTAP","KLAC","ALTR","ADSK","LLTC","AKAM","CTXS",
#              "RHT","CA","NVDA","MCHP","EA","CSC","WU","LRCX","HRS","TDC","FFIV","VRSN","FTR","FLIR","TSS","WIN","FSLR","JBL"
# )
stocklist<-c("XLK","AUY","SPY","AAPL")
# Use the quantmod function to get all of the stock data properly.
getSymbols(stocklist,src="yahoo")



################################################################################
### Possible function for adding technical indicators.
stock <- XLK
names(stock) <- c("Open","High","Low","Close","Volume","Adjusted")
stock$BBand <- as.xts(BBands(Ad(stock))$pctB)
stock$RSI <- as.xts(RSI(Ad(stock)))
stock$PPO <- PPO(stock)$ppoHist
#stock$TRIX <- as.xts(TRIX(Ad(stock))$TRIX)
#stock$TRIXsignal <- as.xts(TRIX(Ad(stock))$signal)
#stock$myTRIX <- TRIX(Ad(stock))$TRIX - TRIX(Ad(stock))$signal
#stock$MACD <- as.xts(MACD(Ad(stock), nFast=12, nSlow=26, nSig=9)$macd)
#stock$MACDsignal <- as.xts(MACD(Ad(stock))$signal)
#stock$myMACD <- MACD(Ad(stock))$macd - MACD(Ad(stock))$signal
#stock$ADX <- ADX(stock, n=14)$ADX
#stock$EVWMA <- EVWMA(Ad(stock))

stock$sigBBand <- ifelse(stock$BBand < -0.1, 1,
                         ifelse(stock$BBand > 1.1,-1,0))

stock$sigRSI <- ifelse(stock$RSI < 30, 1,
                       ifelse(stock$RSI > 70,-1,0))

stock$sigTRIX <- ifelse(stock$myTRIX < 0 & stock$TRIX < 0, 1,
                       ifelse(stock$myTRIX > 0 & stock$TRIX > 0,-1,0))


tail(stock, n=20)
summary(stock$PPO)

# Chart to see what's going on.
chartStock("XLK",90)
#addGuppy()
addTA(PPO(XLK)$ppo)
addTA(PPO(XLK)$signal)
addPPO()
addRSI()
addMACD()
addADX()
addMomentum()
addEVWMA()
addEnvelope()
addExpiry()
addDPO()
addSAR()
addATR()
addTA(OpCl(AUY), col='blue',type='h')
addPPO()

#################################################################################

