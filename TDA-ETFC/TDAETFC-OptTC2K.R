library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
TETFCtxt <- read.table('tdaetfcnew', header = FALSE, sep = ',')
TETFCzoo <- read.zoo(TETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TETFCxts <- as.xts(TETFCzoo)

ETFData <- TETFCxts['2014-11-07:/']

#Calculate daily log component returns
#Asset.Ret <- returns(Portlist.x) *100
Asset.Ret <- na.omit(Return.calculate(ETFData, method = ("discrete")))

#Extract Returns by asset class
# Equity.Ret <-na.omit(cbind(Asset.Ret[,c(1,2,4,9,10)]))
Equity.Ret <-na.omit(cbind(Asset.Ret[,c(1,3,4,7,8,10)])) ## Includes REET as Equity
Fixed.Ret <- cbind(Asset.Ret[,c(2,5,6,9)])
# Real.Ret <- cbind(Asset.Ret[,c(3,5)])

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%

FIXEDroll <- rollingWindows(Fixed.Ret, period = "12m", by = "1m")
# REALroll <- rollingWindows(Real.Ret, period = "12m", by = "1m")
EQroll <- rollingWindows(Equity.Ret, period = "12m", by = "1m")

#### Identify the Components to make up the final Equity and Fixed Income models ####

#Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio Using Rolling Windows#
Fixed.roll <- rollingTangencyPortfolio(as.timeSeries(Fixed.Ret), spec = Default,
                                       constraints = c('minW[1:4] = .1','maxW[1:4] = .5'),
                                       from = FIXEDroll$from, to = FIXEDroll$to)

# Real.roll <- rollingTangencyPortfolio(as.timeSeries(Real.Ret), spec = Default,
#                                       constraints = c('maxW[1:2] = 1'),
#                                       from = REALroll$from, to = REALroll$to)

# Equity.roll <- rollingTangencyPortfolio(as.timeSeries(Equity.Ret), spec = Default,
#                                         constraints = c('minW[1:5] = .05','maxW[1:5] = .35'),
#                                         from = EQroll$from, to = EQroll$to)

Equity.roll <- rollingTangencyPortfolio(as.timeSeries(Equity.Ret), spec = Default,
                                        constraints = c('minW[1:6] = .05','maxW[1:6] = .35'),
                                        from = EQroll$from, to = EQroll$to)

# EQWeights <- read.csv("EquityWts.csv")
# EQWeights.z <- read.zoo("EquityWts.csv", header = TRUE, sep = ",")
# EQWeights.x <- as.xts(EQWeights.z)
# 
# EQRet.ep <- endpoints(Equity.Ret, on = 'months', k=1) 
# EQRet.mo <- Equity.Ret[EQRet.ep]
#  
# EQUITYRet <- round(Return.portfolio(EQRet.mo, weights = EQWeights.x, wealth.index = FALSE, 
#                                     contribution = FALSE, geometric = TRUE), digits = 6)
# 
# table.CalendarReturns(EQUITYRet)
# SharpeRatio(EQUITYRet, .01/12, FUN = "StdDev")
# sd(EQUITYRet)

print(last(Equity.roll))
print(last(Fixed.roll))
# print(last(Real.roll))

