library('quantmod')
setwd("~/GitHub/quantstuff")
######################################################
bbandScanner <- function(a){
   
#   if(exists("signalList")==F) {
      signalList <- as.data.frame(matrix(NA,nrow=0,ncol=3))
      names(signalList) <- c("stock","pctB","signal")
      row<-0
 #  }
   
   for(i in 1:length(a)){
   mySymbol <- toupper(a[i])
   print(mySymbol)
   
   # If the quote object doesn't exist. Get it from Yahoo.
   # If it already exists, this code will be skipped.
   if(exists(mySymbol)==F) {
      getSymbols(mySymbol,src="yahoo")
   }
   
   myStock <- eval(parse(text=mySymbol))
   
   bands <- BBands(Cl(myStock))$pctB
   bands <- tail(bands, n=1)
   bands$signal <- NA
   bands$signal <- ifelse(bands$pctB > 1, "Sell",
               ifelse(bands$pctB < 0,"Buy",
                 "Wait"))
   
   
   row <- nrow(signalList) + 1
   signalList[row,1] <- mySymbol
   signalList[row,2] <- bands$pctB
   signalList[row,3] <- bands$signal
   
   }
   
   }
   ########################################################

do.call(bbandScanner,as.list((c("AAPL"))))

###################################################
