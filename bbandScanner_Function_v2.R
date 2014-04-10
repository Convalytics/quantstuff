#install.packages("PerformanceAnalytics")
library(quantmod)
library(PerformanceAnalytics)
setwd("~/GitHub/quantstuff")
######################################################
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
bbscan <- function(stocks){
#stocklist<-c("XLK","AAPL","GOOG","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB","MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM")
##getSymbols(stocklist,src="yahoo")

signalList <- as.data.frame(matrix(NA,nrow=0,ncol=5))
names(signalList) <- c("stock","Close","rsi","pctB","signal")

stocks <- "AAPL"
for(i in 1:length(stocks)){
testgrab <- bbandScanner(stocks[i])
signalList[i,"stock"] <- stocks[i]
signalList[i,"Close"] <- testgrab$Close
signalList[i,"rsi"]<- testgrab$rsi
signalList[i,"pctB"]<- testgrab$pctB
signalList[i,"signal"] <- testgrab$signal


}
}
#############################################################################
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
    addBBands()
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
tech<-c("XLK","AAPL","MSFT","VZ","IBM","ORCL","QCOM","CSCO","INTC","V","FB",
             "MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM",
             "CTSH","ADBE","GLW","AMAT","MU","TEL","INTU","CTL","SNDK","WDC","STX","ADI","BRCM",
             "FIX","APH","XLNX","FISV","NTAP","KLAC","ALTR","ADSK","LLTC","AKAM","CTXS",
             "RHT","CA","NVDA","MCHP","EA","CSC","WU","LRCX","HRS","TDC","FFIV","VRSN","FTR","FLIR","TSS","WIN","FSLR","JBL"                      
             )

keyMarkets_US <- c("IWC","SLY","SPY","DIA")   # EMM
keyMarkets_Global <- c("ACWI","EFA","CWI","GMM","BIK")
keyMarkets_Bonds <- c("BWX","JNK","LAG","IPE","TLT")
keyMarkets_Commodities <- c("OIL","JJA","PTM","JJM","GLD","SLV","GAZ","PALL","PPLT")
keyMarkets_Currencies <- c("UUP","FXE","FXB","CYB","FXY")
agriculture <- c("JO","NIB","COW","FUE","BAL","JJG","SGG","CORN","JJA")
metals <- c("JJT","JJN","LD","JJU","JJC","GLD","SLV","PALL","PPLT","JJM","WITE")
countries <- c("EWI","EGPT","EWP","EIRL","EWG","EWQ","EWN","EWK","EIS","EWL","ARGT","PLND","EWU","EWO","EWY","EWT")    #TMW
currencies <- c("FXB","FXF","FXE","UUP","FXS","CYB","ICN","FXY","FXC","FXA","BZF")   #, SZR
sectors_US <- c("XLI","XLV","XLB","XLY","XLK","XLF","XLE","XLP","XLU")
leveragedETFs <- c("TNA","SSO","UPRO","FAS","QLD","ERX","UWM","AGQ","DDM","UST","VIX")

stocklist <- c(keyMarkets_US,
               tech,
               keyMarkets_Global,
               keyMarkets_Bonds,
               keyMarkets_Commodities,
               keyMarkets_Currencies,
               agriculture,
               metals,
               countries,
               currencies,
               sectors_US,
               leveragedETFs)

stocklist <- stocklist[!duplicated(stocklist)]  # remove duplicates

#stocklist<-c("FB","AUY","XLK")
# Use the quantmod function to get all of the stock data properly.
getSymbols(stocklist,src="yahoo", warnings = FALSE)


# Scan for buy/sell signals based on Bollinger Bands
scanned <- bbscan("OIL")
subset(scanned, signal!="-")

# Chart to see what's going on.
chartStock("AUY",60)
addATR()
addGuppy()
addRSI()
addMACD()
addADX()

#plot(Return.calculate(last(AUY$AUY.Close,180),method="compound"))


# chart.CumReturns(managers[,c(manager.column, index.columns, 
#                              peer.columns), drop = FALSE], main = 'Cumulative Returns', 
#                  legend.loc = 'topleft', event.lines = risk.dates, event.labels = 
#                     risk.labels, ylog = TRUE, wealth.index = TRUE, colorset = colorset, 
#                  lwd = 2)
# # 
# # #????? getOptionChain("AAPL")
# # #????? buildData(BBands(Cl(XLK)))
# 
# 
# 
# #candleChart(AUY,subset="2014", theme="white");addGuppy()
# #addGuppy(on=-1, col=c(rep("blue",6),rep("black",6)))
# 
# 
# test <- as.xts(merge(AUY,BBands(Cl(AUY))$pctB))
# test$RSI <- as.xts(RSI(Cl(AUY)))
# 
# 
# RSI(Cl(AUY))
