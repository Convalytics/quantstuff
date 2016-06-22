# Quant 

### Stock Lists

setwd("~/GitHub/quantstuff")


library(quantmod)
#library(gridExtra)
#library(ggplot2)
#library(dplyr)
######################################################################################

tech<-c("XLK","AAPL","MSFT","VZ","IBM","ORCL","QCOM","CSCO","INTC","V","FB",
        "MA","EBAY","EMC","TXN","ACN","HPQ","HPE","ADP","YHOO","CRM",
        "CTSH","ADBE","GLW","AMAT","MU","TEL","INTU","CTL","SNDK","WDC","STX","ADI","BRCM",
        "FIX","APH","XLNX","FISV","NTAP","KLAC","ALTR","ADSK","LLTC","AKAM","CTXS",
        "RHT","CA","NVDA","MCHP","EA","CSC","WU","LRCX","HRS","TDC","FFIV","VRSN","FTR","FLIR","TSS","WIN","FSLR","JBL"                      
)

keyMarkets_US <- c("SPY","DIA")   # EMM  "IWC","SLY"(lowvolume)
keyMarkets_Global <- c("ACWI","EFA","CWI","BIK")   # Removed: GMM
keyMarkets_Bonds <- c("BWX","JNK","LAG","IPE","TLT")
keyMarkets_Commodities <- c("OIL","JJA","PTM","JJM","GLD","SLV","GAZ","PALL","PPLT")
keyMarkets_Currencies <- c("UUP","FXE","FXB","CYB","FXY")
agriculture <- c("JO","NIB","COW","BAL","JJG","SGG","CORN","JJA")   # Removed: FUE (low volume)
metals <- c("JJN","LD","JJC","GLD","SLV","PALL","PPLT","JJM","WITE")  # Removed: JJT, JJU (low volume)
countries <- c("EWI","EGPT","EWP","EIRL","EWG","EWQ","EWN","EWK","EIS","EWL","ARGT","PLND","EWU","EWO","EWY","EWT")    #TMW
currencies <- c("FXB","FXF","FXE","UUP","CYB","ICN","FXY","FXC","FXA","BZF")   #, SZR, FXS(LowVolume)
sectors_US <- c("XLI","XLV","XLB","XLY","XLK","XLF","XLE","XLP","XLU")
leveragedETFs <- c("TNA","SSO","UPRO","FAS","QLD","ERX","UWM","AGQ","DDM","UST")
currentHoldings <- c("PAYX")
liquidETFs <- c("XLI","VXX","UNG","UUP","UCO","XRT","EWJ","EWG","EUO","FXI","XHB","TLT","EWA","VWO","DBC","GLD","SPY","USO")  #From: 
favorites <- c("USO","SPY","UUP","TLT","QQQ","GLD")
# stocklist <- c(keyMarkets_US,
#                tech,
#                keyMarkets_Global,
#                keyMarkets_Bonds,
#                keyMarkets_Commodities,
#                keyMarkets_Currencies,
#                agriculture,
#                metals,
#                countries,
#                currencies,
#                sectors_US,
#                leveragedETFs,
#                currentHoldings)
#stocklist <- c(leveragedETFs, currentHoldings)

#stocklist<-c("USO","SPY","UUP")
# Use the quantmod function to get all of the stock data properly.

stocklist <- c(currentHoldings)#, favorites, liquidETFs, keyMarkets_US, currencies, sectors_US)
stocklist <- stocklist[!duplicated(stocklist)]  # remove duplicates
getSymbols(stocklist,src="yahoo", warnings = FALSE)
#getQuote("GLD")
#tail(SPY)

########################################################################


# payxNew <- fortify(PAYX["2016/"])
# payxChart <- ggplot(payxNew, aes(x=Index, y=PAYX.Adjusted)) + geom_line() + labs(x="", y="", title="PAYX (Paychex)")

usoNew <- fortify(USO["2016/"])
usoChart <- ggplot(usoNew, aes(x=Index, y=USO.Adjusted)) + geom_line()   + labs(x="", y="", title="USO (U.S. Oil)")

spyNew <- fortify(SPY["2016/"])
spyChart <- ggplot(spyNew, aes(x=Index, y=SPY.Adjusted)) + geom_line()   + labs(x="", y="", title="SPY (SPDR S&P 500 ETF)")

uupNew <- fortify(UUP["2016/"])
uupChart <- ggplot(uupNew, aes(x=Index, y=UUP.Adjusted)) + geom_line()   + labs(x="", y="", title="UUP (US Dollar Index)")

tltNew <- fortify(TLT["2016/"])
tltChart <- ggplot(tltNew, aes(x=Index, y=TLT.Adjusted)) + geom_line()   + labs(x="", y="", title="TLT (20+ Yr Treas Bond)")

gldNew <- fortify(GLD["2016/"])
gldChart <- ggplot(gldNew, aes(x=Index, y=GLD.Adjusted)) + geom_line()   + labs(x="", y="", title="GLD (SPDR Gold Trust ETF)")


grid.arrange(tltChart, usoChart, spyChart, uupChart, gldChart, ncol=3)
##########################################################################

##########################################################################
# library(dplyr)
# library(ggplot2)
# head(GLD)
# GLD$GLD.PctChange <- (GLD$GLD.Close - GLD$GLD.Open) / GLD$GLD.Open
# head(fortify(GLD))  #fortify being deprecated for the broom package. Doesn't seem to work the same though.




# http://www.quantmod.com/documentation/specifyModel.html
source(file="2_QuantFunctions.R")

# testStock <- addStockData(SPY)
# tail(fortify(testStock))

spyData <- addStockData(SPY)
spyData$symbol <- 'SPY'

gldData <- addStockData(GLD)
gldData$symbol <- 'GLD'

tltData <- addStockData(TLT)
tltData$symbol <- 'TLT'

usoData <- addStockData(USO)
usoData$symbol <- 'USO'

uupData <- addStockData(UUP)
uupData$symbol <- 'UUP'

qqqData <- addStockData(QQQ)
qqqData$symbol <- 'QQQ'

payxData <- addStockData(PAYX)
payxData$symbol <- 'PAYX'



allStocks <- rbind(spyData,gldData, tltData, usoData, uupData, qqqData, payxData)
#rm(testStock)
#plot(testStock$GAP_Pct ~ testStock$MaxProfit_1d_Pct)

ggplot(allStocks, aes(ClCl_Pct, AvgProfit_1d_Pct, color=Body_Color)) + 
  geom_point(alpha=.25) + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=0)  
#  geom_smooth()

#(Next(Hi(TLT),k=1) - Cl(TLT))


