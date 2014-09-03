quantstuff
==========
## QuantStuff
#  Retrieves the data for a list of stocks, and will return buy/sell signals for each. 

https://github.com/Convalytics/quantstuff


You’ll have to modify the working directory to a real location on your pc, but everything else should just work.  
Change this path:  setwd("~/GitHub/quantstuff/quantProject")
I’m working on building this into a package that you can simply install…

1.	Open StockAnalysisFunctions.R 
a.	Select all and run the entire file.  (This puts all of the functions into your global environment.)
2.	Open StockAnalysis_LoadData.R
a.	Again, you can run the entire file, or add/remove stock symbols as you see fit.
b.	This one gets all of the stock data from yahoo. *There’s a 1-second pause between requests, so it can take a while to do many symbols.
3.	Open StockAnalysis.R
a.	This is the file where we use all of our newly-downloaded data.
b.	Lines 10 & 11 run the scanner function against our list and writes the results to a csv file.  Sort by “mySignal” and I usually add a little conditional formatting to visualize the highs and lows by column.
c.	Run lines 15 through 26 to get a chart for a specific stock. 
i.	Just change the “mySymbol” value (Uppercase!) to the stock you’d like to review, and the “days” to the time span you want to cover.


-- Jason Green
-- Last Updated: 9/3/2014
