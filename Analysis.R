library('quantmod')
library('knitr')
#library('zoo')
#library('xts')
#library('PerformanceAnalytics')
setwd("~/GitHub/quantstuff")
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
   addBBands()
   addTA(BBands(Cl(myStock))$pctB)
   #addMACD()
 #####  todayQuote
}
########################################################

### Symbol groups
techSector <- c("XLK","AAPL","GOOG","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB","MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM")

# Load a bunch of symbols at once:
getSymbols(techSector,src="yahoo")
#loadSymbols <- c("XPH","XBI","IHF","IHI","JO","MSFT")
##########################################################
# Run everything above to initialize
##########################################################


chartStock("amzn","300")
chartStock("fb","60")
chartStock("fb","90")
chartStock("fb","180")


