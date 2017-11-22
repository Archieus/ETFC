library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
TETFCtxt <- read.table('tdetfc', header = FALSE, sep = ',')
TETFCzoo <- read.zoo(TETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TETFCxts <- as.xts(TETFCzoo)

ETFData <- TETFCxts['2014-11-07:/']

#Calculate daily log component returns
#Asset.Ret <- returns(Portlist.x) *100
Asset.Ret <- na.omit(Return.calculate(ETFData, method = ("discrete")))

#Extract Returns by asset class
Equity.Ret <-na.omit(cbind(Asset.Ret[,c(6,7,15,16,21)]))
Fixed.Ret <- cbind(Asset.Ret[,c(9,19,23,25)])

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%
# 
# FIXEDroll <- rollingWindows(Fixed.Ret, period = "12m", by = "1m")
# EQroll <- rollingWindows(Equity.Ret, period = "12m", by = "1m")

#### Identify the Components to make up the final Equity and Fixed Income models ####

# #Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio Using Rolling Windows#
# Fixed.roll <- rollingTangencyPortfolio(as.timeSeries(Fixed.Ret), spec = Default,
#                                        constraints = c('minW[1:4] = .05'),
#                                        from = FIXEDroll$from, to = FIXEDroll$to)
# 
# 
# Equity.roll <- rollingTangencyPortfolio(as.timeSeries(Equity.Ret), spec = Default,
#                                         constraints = c('maxW[1:5] = .35'),
#                                         from = EQroll$from, to = EQroll$to)

Equity.tp <- tangencyPortfolio(as.timeSeries(Equity.Ret['2015-12-01:/2016-11-30']), spec = Default,
                               constraints = c('minW[1:5] = .05', 'maxW[1:5] = .35'))

Fixed.tp <- tangencyPortfolio(as.timeSeries(Fixed.Ret['2015-12-01:/2016-11-30']), spec = Default,
                               constraints = c('maxW[1:4] =.35'))

Fixed.tp
Equity.tp

