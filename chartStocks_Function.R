library('quantmod')

chartStock <- function(a,b){
mySymbol <- toupper(a)
dayRange <- as.numeric(b)

getSymbols(mySymbol,src="yahoo")
todayQuote <- getQuote(mySymbol, what=yahooQuote.EOD)
myStock <- eval(parse(text=mySymbol))


#candleChart(last(mySymbol,dayRange),dn.col="black",up.col="white",theme="white")
chartSeries(last(myStock,dayRange),
            type="candlesticks",
            name=paste0(mySymbol, " - Past ", dayRange, " Days"),
            theme=chartTheme("white"),
            up.col="white",
            dn.col="black"     
)
addBBands()
addMACD()
todayQuote
}

chartStock("aapl","150")
