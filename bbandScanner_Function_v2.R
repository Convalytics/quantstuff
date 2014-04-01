library('quantmod')
setwd("~/GitHub/quantstuff")
######################################################
bbandScanner <- function(a){
   
   mySymbol <- toupper(a)
   print(mySymbol)
   
   # If the quote object doesn't exist. Get it from Yahoo.
   # If it already exists, this code will be skipped.
#    if(exists(mySymbol)==F) {
#       getSymbols(mySymbol,src="yahoo")
#    }
   
   myStock <- eval(parse(text=mySymbol))
   
   bands <- BBands(Cl(myStock))$pctB
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
#stocklist<-c("XLK","AAPL","GOOG","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB","MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM")
##getSymbols(stocklist,src="yahoo")

signalList <- as.data.frame(matrix(NA,nrow=0,ncol=3))
names(signalList) <- c("stock","pctB","signal")

for(i in 1:length(stocklist)){
testgrab <- bbandScanner(stocklist[i])
signalList[i,"stock"]<-stocklist[i]
signalList[i,"pctB"]<-testgrab$pctB
signalList[i,"signal"]<-testgrab$signal
}
signalList
}
#############################################################################
#############################################################################
######################################################
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
   
   addTA(BBands(Cl(myStock))$pctB)
   addTA(BBands(Cl(myStock)),on=1)
   #addBBands()
   #addMACD()
   #####  todayQuote
}
########################################################
#####################################################################
### End Functions
#################

stocklist<-c("AUY","XLK","AAPL","GOOG","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB",
             "MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM",
             "CTSH","ADBE","GLW","AMAT","MU","TEL","INTU","CTL","SNDK","WDC","STX","ADI","BRCM",
             "FIX","APH","XLNX","FISV","NTAP","KLAC","ALTR","ADSK","LLTC","AKAM","CTXS",
             "RHT","CA","NVDA","MCHP","EA","CSC","WU","LRCX","HRS","TDC","FFIV","VRSN","FTR","FLIR","TSS","WIN","FSLR","JBL"
             )

# Use the quantmod function to get all of the stock data properly.
getSymbols(stocklist,src="yahoo")


# Scan for buy/sell signals based on Bollinger Bands
bbscan(stocklist)

# Chart to see what's going on.
chartStock("FLIR",60)
