### Stock Analysis

setwd("~/GitHub/quantstuff/quantProject")

library(quantmod)
#source(file="StockAnalysisFunctions.R")
require(quantstuff)
######################################################################################
############# SCAN ALL STOCKS ########################################################

scanned <- bbscan(stocklist)
subset(scanned, signal!="-")
head(scanned)

############# GET STOCK DETAILS ########################################################
mySymbol <- "UUP"
days <- 30
myStock <- eval(parse(text=mySymbol))


thisStock <- getSignals(mySymbol, days, myStock)
tail(thisStock)

############# CHART THE SELECTED STOCK ########################################################
getChart(mySymbol,days,thisStock)
addEMA(n=3)
addEMA(n=30)
addBBands()

addRSI()
addTA(thisStock$BBand_pctB)
addTA(thisStock$PPO)
#addATR()
#addMomentum()
#tail((EMA(Cl(stockData), n=3) - EMA(Cl(stockData), n=30))/EMA(Cl(stockData), n=30), n=30)
