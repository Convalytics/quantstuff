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



#???  attachSymbols()

######################################################################
######################################################################
### End Functions
######################################################################
######################################################################




### Goog split causing issues???
stocklist<-c("AUY","XLK","AAPL","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB")
#              "MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM",
#              "CTSH","ADBE","GLW","AMAT","MU","TEL","INTU","CTL","SNDK","WDC","STX","ADI","BRCM",
#              "FIX","APH","XLNX","FISV","NTAP","KLAC","ALTR","ADSK","LLTC","AKAM","CTXS",
#              "RHT","CA","NVDA","MCHP","EA","CSC","WU","LRCX","HRS","TDC","FFIV","VRSN","FTR","FLIR","TSS","WIN","FSLR","JBL"
# )
#stocklist<-c("FB","AUY","GLD")
# Use the quantmod function to get all of the stock data properly.
getSymbols(stocklist,src="yahoo")



# Chart to see what's going on.
chartStock("AUY",120)
addGuppy()
addRSI()

?addTRIX
#

test$BBand <- as.xts(BBands(Cl(AUY))$pctB)
test$RSI <- as.xts(RSI(Cl(AUY)))
test$TRIX <- as.xts(TRIX(Cl(AUY))$TRIX)
test$TRIXsignal <- as.xts(TRIX(Cl(AUY))$signal)
test$MACD <- as.xts(MACD(Cl(AUY))$macd)
test$MACDsignal <- as.xts(MACD(Cl(AUY))$signal)



tail(MACD(Cl(AUY)))
