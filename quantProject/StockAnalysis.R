### Stock Analysis

setwd("~/GitHub/quantstuff/quantProject")

library(quantmod)
source(file="StockAnalysisFunctions.R")
######################################################################################
############# SCAN ALL STOCKS ########################################################

scanned <- scanner(stocklist)
write.csv(scanned,"TodaysPicks.csv")
##### how to include today's current values as part of the scan?

############# GET STOCK DETAILS ########################################################
mySymbol <- "XLU"
days <- 60
myStock <- eval(parse(text=mySymbol))
tail(myStock)

#chartSeries(last(ERX,60))
############# CHART THE SELECTED STOCK ########################################################
getChart(mySymbol,days,myStock)
addEMA(n=3)
addEMA(n=30)
addBBands()
addRSI()

#AAPL.OPT <- getOptionChain("AAPL")
#needs work: addPoints(Cl(last(myStock,days)),pch=25,col='red',offset=1.03)

# 
# addTA(thisStock$BBand_pctB)
# addTA(thisStock$PPO)


#addATR()
#addMomentum()
#tail((EMA(Cl(stockData), n=3) - EMA(Cl(stockData), n=30))/EMA(Cl(stockData), n=30), n=30)
