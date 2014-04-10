### Stock Analysis

setwd("~/GitHub/quantstuff/quantProject")

library(quantmod)
source(file="StockAnalysisFunctions.R")
######################################################################################
############# SCAN ALL STOCKS ########################################################

scanned <- scanner(stocklist)

############# GET STOCK DETAILS ########################################################
mySymbol <- "UST"
days <- 200
myStock <- eval(parse(text=mySymbol))
#tail(BBands(Cl(ERX)))

#chartSeries(last(ERX,60))
############# CHART THE SELECTED STOCK ########################################################
getChart(mySymbol,days,myStock)
addEMA(n=3)
addEMA(n=30)
addBBands()

addRSI()
addTA(thisStock$BBand_pctB)
addTA(thisStock$PPO)
#addATR()
#addMomentum()
#tail((EMA(Cl(stockData), n=3) - EMA(Cl(stockData), n=30))/EMA(Cl(stockData), n=30), n=30)
