### Stock Lists

setwd("~/GitHub/quantstuff/quantProject")

library(quantmod)

######################################################################################

tech<-c("XLK","AAPL","MSFT","VZ","IBM","ORCL","QCOM","CSCO","INTC","V","FB",
        "MA","EBAY","EMC","TXN","ACN","HPQ","ADP","YHOO","CRM",
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
currentHoldings <- c("AUY","XLK","INTU")
liquidETFs <- c("XLI","VXX","UNG","UUP","UCO","XRT","EWJ","EWG","EUO","FXI","XHB","TLT","EWA","VWO","DBC","GLD","SPY")  #From: 

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
stocklist <- liquidETFs
#stocklist<-c("FB","AUY","XLK")
# Use the quantmod function to get all of the stock data properly.
   
   stocklist <- stocklist[!duplicated(stocklist)]  # remove duplicates
   getSymbols(stocklist,src="yahoo", warnings = FALSE)
getQuote("GLD")
