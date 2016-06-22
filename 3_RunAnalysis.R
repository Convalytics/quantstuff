### Stock Analysis

setwd("~/GitHub/quantstuff")

library(quantmod)
source(file="2_QuantFunctions.R")
######################################################################################
############# SCAN ALL STOCKS ########################################################

scanned <- scanner(stocklist)
write.csv(scanned,"TodaysPicks.csv")
##### how to include today's current values as part of the scan?

############# GET STOCK DETAILS ########################################################
mySymbol <- "PAYX"
days <- 250
myStock <- eval(parse(text=mySymbol))
#tail(myStock)
#last(PAYX,10)
#chart_Series(last(PAYX,10))
############# CHART THE SELECTED STOCK ########################################################
getChart(mySymbol,days,myStock)
#addEMA(n=5)
addSMA(n=50)
addSMA(n=100)
addRSI()
addBBands()
addMACD(50,100,50)
addGuppy()
addTA(Vo(myStock) * Cl(myStock), legend="Dollars Traded")
#AAPL.OPT <- getOptionChain("AAPL")
#needs work: addPoints(Cl(last(myStock,days)),pch=25,col='red',offset=1.03)

# 
# addTA(thisStock$BBand_pctB)
# addTA(thisStock$PPO)


#addATR()
#addMomentum()
#tail((EMA(Cl(stockData), n=3) - EMA(Cl(stockData), n=30))/EMA(Cl(stockData), n=30), n=30)


