library('quantmod')
#library('zoo')
#library('xts')
#library('PerformanceAnalytics')

techSector <- c("XLK","AAPL","GOOG") #,"MSFT","VZ","IBM","T","ORCL","QCOM","CSCO","INTC","V","FB","MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM")
#loadSymbols <- c("XPH","XBI","IHF","IHI","JO","MSFT")

getSymbols(techSector,src="yahoo")

mySymbol <- XLK
dayRange <- 500

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

#chartStock("XLK","90")
getChart("XLK",500,XLK)
#My Indicators
# not used: addTA(LoHi(last(mySymbol,dayRange)))
#Absolute daysBody <- abs(XLK$XLK.Close - XLK$XLK.Open)  # Body Size
daysBody <- XLK$XLK.Close - XLK$XLK.Open  # Body Size
daysRange <- XLK$XLK.High - XLK$XLK.Low   # Low to High
daysUpShadow <- ifelse(XLK$XLK.Close > XLK$XLK.Open, XLK$XLK.High - XLK$XLK.Close, XLK$XLK.High - XLK$XLK.Open)      # Upper Shadow Size
daysLoShadow <- ifelse(XLK$XLK.Close > XLK$XLK.Open, XLK$XLK.Open - XLK$XLK.Low, XLK$XLK.Close - XLK$XLK.Low)     # Lower Shadow Size
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
