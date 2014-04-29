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

keyMarkets_US <- c("IWC","SLY","SPY","DIA")   # EMM
keyMarkets_Global <- c("ACWI","EFA","CWI","GMM","BIK")
keyMarkets_Bonds <- c("BWX","JNK","LAG","IPE","TLT")
keyMarkets_Commodities <- c("OIL","JJA","PTM","JJM","GLD","SLV","GAZ","PALL","PPLT")
keyMarkets_Currencies <- c("UUP","FXE","FXB","CYB","FXY")
agriculture <- c("JO","NIB","COW","FUE","BAL","JJG","SGG","CORN","JJA")
metals <- c("JJT","JJN","LD","JJU","JJC","GLD","SLV","PALL","PPLT","JJM","WITE")
countries <- c("EWI","EGPT","EWP","EIRL","EWG","EWQ","EWN","EWK","EIS","EWL","ARGT","PLND","EWU","EWO","EWY","EWT")    #TMW
currencies <- c("FXB","FXF","FXE","UUP","FXS","CYB","ICN","FXY","FXC","FXA","BZF")   #, SZR
sectors_US <- c("XLI","XLV","XLB","XLY","XLK","XLF","XLE","XLP","XLU")
leveragedETFs <- c("TNA","SSO","UPRO","FAS","QLD","ERX","UWM","AGQ","DDM","UST","VIX")
currentHoldings <- c("AUY","XLK","INTU")
stocklist <- c(keyMarkets_US,
               tech,
               keyMarkets_Global,
               keyMarkets_Bonds,
               keyMarkets_Commodities,
               keyMarkets_Currencies,
               agriculture,
               metals,
               countries,
               currencies,
               sectors_US,
               leveragedETFs,
               currentHoldings)
#stocklist <- c(leveragedETFs, currentHoldings)
stocklist <- stocklist[!duplicated(stocklist)]  # remove duplicates

#stocklist<-c("FB","AUY","XLK")
# Use the quantmod function to get all of the stock data properly.
getSymbols(stocklist,src="yahoo", warnings = FALSE)
