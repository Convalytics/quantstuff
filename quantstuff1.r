######################################################
# Quant Stuff
# Jason Green
# Exploring stock charting with quantmod.
# File created: 3/30/3014
######################################################

#install.packages("quantmod")
# Load Packages
library(plyr)
library(ggplot2)
library(gridExtra)
library(quantmod)


# Set Working Directory
setwd("~/GitHub/quantstuff")


# Choose a symbol
symbol <- "AUY"
getSymbols(symbol)
# Get stock data
#SPY
tail(eval(parse(text=symbol)))
# Display by day, week, month, summary?


chartSeries(eval(parse(text=symbol)),
            type =  "candlesticks",             #c("auto", "candlesticks", "matchsticks", "bars","line"), 
            subset = "last 4 months",
            show.grid = TRUE, 
            #name = NULL,
            #time.scale = NULL,
            TA = 'addVo()',
            #TAsep=';',
            #line.type = "l",
            bar.type = "ohlc",
            theme = chartTheme("black"),
            #layout = NA,
            major.ticks='auto', minor.ticks=TRUE
            )