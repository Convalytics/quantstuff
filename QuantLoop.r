library('quantmod')
#library('zoo')
#library('xts')



#Rules & Settings ------------------------------------------------------------------------
mySymbol <- AUY
dayRange <- 90

daysBodyRule <- .02
daysRangeRule <- .08
daysUpperShadowRule <- .03
daysLowerShadowRule <- .08

prevDaysBodyRule <- .02
prevDaysRangeRule <- .08
prevDaysUpperShadowRule <- .02
prevDaysLowerShadowRule <- .02

#end rules --------------------------------------------------------------------

# ETF List: http://en.wikipedia.org/wiki/List_of_American_exchange-traded_funds#Small-cap_ETFs

smallCapETFs <- c("IWM","IWN","IWO","IJR","IJT","IJS","VIOO","VIOG","VIOV","VB","VBK","VBR","VTWO","VTWG","VTWV","VXF")
majorETFs <- c("SPY","DIA","IOO","IVV","VOO","VXF","VONE","VTWO","IWM","OEF","QQQ","WXSP")
testList <- c("SPY","AUY","DIA")


# end stock lists -------------------------------------------------------------



# Load Stock Data
loadSymbols <- testList

# add Try/Catch logic
getSymbols(loadSymbols,src="yahoo")






#My Indicators
#Today's Numbers -----------------------------------------------------------
daysBodyAbs <- abs(Cl(mySymbol) - Op(mySymbol))/Cl(mySymbol)  # Body Size
daysBody <- (Cl(mySymbol) - Op(mySymbol))/Cl(mySymbol)  # Body Size
daysRange <- (Hi(mySymbol) - Lo(mySymbol))/Hi(mySymbol)   # Low to High
daysUpShadow <- ifelse(Cl(mySymbol) > Op(mySymbol), 
                       (Hi(mySymbol) - Cl(mySymbol))/Hi(mySymbol), 
                       (Hi(mySymbol) - Op(mySymbol))/Hi(mySymbol))      # Upper Shadow Size
daysLoShadow <- ifelse(Cl(mySymbol) > Op(mySymbol), 
                       (Op(mySymbol) - Lo(mySymbol))/Op(mySymbol), 
                       (Cl(mySymbol) - Lo(mySymbol))/Cl(mySymbol))     # Lower Shadow Size
daySummary <- daysBodyAbs + daysRange + daysUpShadow + daysLoShadow
shadowLength <- daysRange - daysBodyAbs
#----------------------------------------------------------

#Yesterday's Numbers ----------------------------------------------------
prevDaysBodyAbs <- abs(Lag(Cl(mySymbol)) - Lag(Op(mySymbol)))/Lag(Cl(mySymbol))  # Body Size
prevDaysBody <- (Lag(Cl(mySymbol)) - Lag(Op(mySymbol)))/Lag(Cl(mySymbol))  # Body Size
prevDaysRange <- (Lag(Hi(mySymbol)) - Lag(Lo(mySymbol)))/Lag(Hi(mySymbol))   # Low to High
prevDaysUpShadow <- ifelse(Lag(Cl(mySymbol)) > Lag(Op(mySymbol)), 
                           (Lag(Hi(mySymbol)) - Lag(Cl(mySymbol))) / Lag(Hi(mySymbol)), 
                           (Lag(Hi(mySymbol)) - Lag(Op(mySymbol)))/Lag(Hi(mySymbol)))      # Upper Shadow Size
prevDaysLoShadow <- ifelse(Lag(Cl(mySymbol)) > Lag(Op(mySymbol)), 
                           (Lag(Op(mySymbol)) - Lag(Lo(mySymbol))) / Lag(Op(mySymbol)), 
                           (Lag(Cl(mySymbol)) - Lag(Lo(mySymbol))) / Lag(Cl(mySymbol)))     # Lower Shadow Size
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

overallScore <- todayScore + prevDayScore





## --------------------------------------
## Charting -------------------------------------------------------------------------

candleChart(last(mySymbol,dayRange),dn.col="black",up.col="white",theme="white")

addBBands()
addMACD()

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
