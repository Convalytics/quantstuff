library('quantmod')
#library('zoo')
#library('xts')
#library('PerformanceAnalytics')

techSector <- c("XLK","AAPL","GOOG","MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB","MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM")
#loadSymbols <- c("XPH","XBI","IHF","IHI","JO","MSFT")

getSymbols(techSector,src="yahoo")

mySymbol <- AUY
dayRange <- 90

#candleChart(last(mySymbol,dayRange),dn.col="black",up.col="white",theme="white")
chartSeries(last(mySymbol,dayRange),
            type="candlesticks",
            name="XLK Tech Sector SPDR Fund",
            theme=chartTheme("white"),
            up.col="white",
            dn.col="black",
            grid.col="gray"         
            )
addBBands()
addMACD()

chartStock("XLK","90")
#My Indicators
# not used: addTA(LoHi(last(mySymbol,dayRange)))
#Absolute daysBody <- abs(AUY$AUY.Close - AUY$AUY.Open)  # Body Size
daysBody <- AUY$AUY.Close - AUY$AUY.Open  # Body Size
daysRange <- AUY$AUY.High - AUY$AUY.Low   # Low to High
daysUpShadow <- ifelse(AUY$AUY.Close > AUY$AUY.Open, AUY$AUY.High - AUY$AUY.Close, AUY$AUY.High - AUY$AUY.Open)      # Upper Shadow Size
daysLoShadow <- ifelse(AUY$AUY.Close > AUY$AUY.Open, AUY$AUY.Open - AUY$AUY.Low, AUY$AUY.Close - AUY$AUY.Low)     # Lower Shadow Size
daySummary <- daysBody + daysRange + daysUpShadow + daysLoShadow
addTA(last(daysBody,dayRange), col="blue", overlay=TRUE)
addTA(last(daysRange,dayRange), col="brown")
addTA(last(daysUpShadow,dayRange), col="green")
addTA(last(daysLoShadow,dayRange), col="red")
addTA(last(daySummary,dayRange), col="purple")

# Cross Correlation
#ccf(last(drop(AUY$AUY.Open), 90),last(drop(daysRange), 90))
#ccf(last(drop(AUY$AUY.Open), 400),last(drop(GLD$GLD.Open), 400))

#Plot various metrics...
#plot(last(daysRange,90),col="blue")
#lines(last(daysBody,90),col="red")
