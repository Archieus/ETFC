library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
ETFCtxt <- read.table('ETFC', header = FALSE, sep = ',')
ETFCzoo <- read.zoo(ETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ETFCxts <- as.xts(ETFCzoo)

Portlist.x <- na.omit(cbind(ETFCxts[,c(1:5,16:20)]))

#Calculate daily log component returns

Asset.Ret <- na.omit(Return.calculate(Portlist.x, method = ("discrete")))

#Extract Returns by asset class
Equity.Ret <-na.omit(cbind(Asset.Ret[,c(5:7,9:10)]))

EQWeights <- read.csv("oldetfwts.csv")
EQWeights.z <- read.zoo("oldetfwts.csv", header = TRUE, sep = ",")
EQWeights.x <- as.xts(EQWeights.z)

EQRet.ep <- endpoints(Equity.Ret, on = 'months', k=1) 
EQRet.mo <- Equity.Ret[EQRet.ep]

EQUITYRet <- round(Return.portfolio(EQRet.mo, weights = EQWeights.x, wealth.index = FALSE, 
                                    contribution = FALSE, geometric = TRUE), digits = 6)

table.CalendarReturns(EQUITYRet)
SharpeRatio(EQUITYRet, .01/12, FUN = "StdDev")
sd(EQUITYRet)
