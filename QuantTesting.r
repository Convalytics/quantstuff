library('quantmod')
#library('zoo')
#library('xts')



# ETF List: http://en.wikipedia.org/wiki/List_of_American_exchange-traded_funds#Small-cap_ETFs

smallCapETFs <- c("IWM","IWN","IWO","IJR","IJT","IJS","VIOO","VIOG","VIOV","VB","VBK","VBR","VTWO","VTWG","VTWV","VXF")
majorETFs <- c("SPY","DIA","IOO","IVV","VOO","VXF","VONE","VTWO","IWM","OEF","QQQ","WXSP")
testList <- c("SPY","AAPL","DIA","AUy'")

# end stock lists -------------------------------------------------------------

#for each symbol
for(stock in (testList)) {
  # Do stuff
  print(stock)
  todayQuote <- getQuote(stock,what=yahooQF(c("Open","Days High","Days Low","Last Trade (Price Only)","Previous Close")))
  print(todayQuote)
  
  RangeTest <- todayQuote$Last > (todayQuote$Open * 1.005)
  CloseOpenTest <- todayQuote$"P. Close" > todayQuote$Open
  CloseLastTest <- todayQuote$"P. Close" < todayQuote$Last
}





# Load Stock Data
loadSymbols <- smallCapETFs    #used to choose which symbols we'll be working with.
#getSymbols(loadSymbols,src="yahoo")
todayQuote <- getQuote(loadSymbols,what=yahooQF(c("Open","Days High","Days Low","Last Trade (Price Only)","Previous Close")))
#standardQuote()
#yahooQF()


#ifelse(last(todayQuote$Last) > (last(todayQuote$Open)), print("Good"),print("Bad"))


# Is today's price up and with a good range?
test <- todayQuote$Last > (todayQuote$Open * 1.005)
test
todayQuote$Last
todayQuote$Open




mySymbol <- AB
dayRange <- 90

candleChart(last(mySymbol,dayRange),dn.col="black",up.col="white",theme="white")

addBBands()
addMACD()

#My Indicators
#Today's Numbers -----------------------------------------------------------
daysBodyAbs <- abs(todayQuote$Last - todayQuote$Open)/todayQuote$Last  # Body Size
daysBody <- (todayQuote$Last - todayQuote$Open)/todayQuote$Last  # Body Size
daysRange <- (todayQuote$High - todayQuote$Low)/todayQuote$High   # Low to High
daysUpShadow <- ifelse(todayQuote$Last > todayQuote$Open, 
                       (todayQuote$High - todayQuote$Last)/todayQuote$High, 
                       (todayQuote$High - todayQuote$Open)/todayQuote$High)      # Upper Shadow Size
daysLoShadow <- ifelse(todayQuote$Last > todayQuote$Open, 
                       (todayQuote$Open - todayQuote$Low)/todayQuote$Open, 
                       (todayQuote$Last - todayQuote$Low)/todayQuote$Last)     # Lower Shadow Size
daySummary <- daysBodyAbs + daysRange + daysUpShadow + daysLoShadow
shadowLength <- daysRange - daysBodyAbs
#----------------------------------------------------------

#Yesterday's Numbers ----------------------------------------------------
prevDaysBodyAbs <- abs(last(Cl(mySymbol)) - last(Op(mySymbol)))/last(Cl(mySymbol))  # Body Size
prevDaysBody <- (last(Cl(mySymbol)) - last(Op(mySymbol)))/last(Cl(mySymbol))  # Body Size
prevDaysRange <- (last(Hi(mySymbol)) - last(Lo(mySymbol)))/last(Hi(mySymbol))   # Low to High
prevDaysUpShadow <- ifelse(last(Cl(mySymbol)) > last(Op(mySymbol)), 
                           (last(Hi(mySymbol)) - last(Cl(mySymbol))) / last(Hi(mySymbol)), 
                           (last(Hi(mySymbol)) - last(Op(mySymbol)))/last(Hi(mySymbol)))      # Upper Shadow Size
prevDaysLoShadow <- ifelse(last(Cl(mySymbol)) > last(Op(mySymbol)), 
                           (last(Op(mySymbol)) - last(Lo(mySymbol))) / last(Op(mySymbol)), 
                           (last(Cl(mySymbol)) - last(Lo(mySymbol))) / last(Cl(mySymbol)))     # Lower Shadow Size
prevDaySummary <- prevDaysBodyAbs + prevDaysRange + prevDaysUpShadow + prevDaysLoShadow
prevDayShadowLength <- prevDaysRange - prevDaysBodyAbs
#----------------------------------------------------------


### Today's Decisions ----------------------------------------------------
daysBodyDecision <- ifelse(daysBodyAbs > daysBodyRule,1,0)
daysRangeDecision <- ifelse(daysRange > daysRangeRule,1,0)
daysUpShadowDecision <- ifelse(daysUpShadow > daysUpperShadowRule,1,0)
daysLoShadowDecision <- ifelse(daysLoShadow < daysLowerShadowRule,1,0)
todayScore <- (daysBodyDecision + 
                 daysRangeDecision +
                 daysUpShadowDecision +
                 daysLoShadowDecision)
#-------------------------------------------------------------

### Prev Day's Decisions ----------------------------------------------------
prevDaysBodyDecision <- ifelse(prevDaysBodyAbs > prevDaysBodyRule,1,0)
prevDaysRangeDecision <- ifelse(prevDaysRange > prevDaysRangeRule,1,0)
prevDaysUpShadowDecision <- ifelse(prevDaysUpShadow > prevDaysUpperShadowRule,1,0)
prevDaysLoShadowDecision <- ifelse(prevDaysLoShadow < prevDaysLowerShadowRule,1,0)
prevDayScore <- (prevDaysBodyDecision + 
                   prevDaysRangeDecision +
                   prevDaysUpShadowDecision +
                   prevDaysLoShadowDecision)
#-------------------------------------------------------------

overallScore <- todayScore + prevDayScore

ifelse(overallScore > 0,print("Yay"),"No")

#--Bullish Engulfing Pattern --------#Rules are in negative order -----------
BullishEngulfing <- ifelse(
  prevDaysBody > -0.001, 0,
  ifelse(prevDaysBody < -.02, 0,
         ifelse(daysBody < prevDaysBodyAbs, 0,
                ifelse(Op(mySymbol)>Lag(Op(mySymbol)), 0,
                       ifelse(Op(mySymbol)>Lag(Cl(mySymbol)), 0, 
                              ifelse(Cl(mySymbol)<Lag(Op(mySymbol)), 0, 
                                     ifelse(Cl(mySymbol)<Lag(Op(mySymbol)), 0, 1
                                     )))))))
#-------------------------------------------------------------




addTA(last(BullishEngulfing,dayRange), col="blue") # test

# addTA(last(daysBodyDecision,dayRange), col="blue")
# addTA(last(daysRangeDecision,dayRange), col="brown")
# addTA(last(daysUpShadowDecision,dayRange), col="green")
# addTA(last(daysLoShadowDecision,dayRange), col="red")
addTA(last(overallScore,dayRange))

# 
# 
# addTA(last(daysBody,dayRange), col="blue")
# addTA(last(daysRange,dayRange), col="brown")
# addTA(last(daysUpShadow,dayRange), col="green")
# addTA(last(daysLoShadow,dayRange), col="red")
# addTA(last(daySummary,dayRange), col="purple")
# addTA(last(shadowLength,dayRange))
# 
# # Cross Correlation
# #ccf(last(drop(SPY$SPY.Open), 90),last(drop(daysRange), 90))
# #ccf(last(drop(SPY$SPY.Open), 400),last(drop(GLD$GLD.Open), 400))
# ccf(last(drop(daysRange),dayRange),last(drop(ClCl(SPY)),dayRange))
# 
# 
# #Plot various metrics...
# #plot(last(daysRange,90),col="blue")
# #lines(last(daysBody,90),col="red")
# 
# 
# plot(last(dailyReturn(SPY),dayRange))
# lines(last(OpCl(SPY),dayRange),col="blue")
# daySummary
# par(bg="gray")
# plot(last(daysUpShadow,dayRange),lwd=2,col="green")
# lines(last(ClCl(SPY),dayRange)*100,lwd=2)
# lines(last(daysLoShadow,dayRange),lwd=2, col="red")
