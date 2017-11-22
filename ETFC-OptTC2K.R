library(PerformanceAnalytics)
library(fPortfolio)
library(quantmod)
Sys.setenv(TZ = 'GMT')

#### reads/loads text into R and converts to XTS ####
ETFCtxt <- read.table('ETFC', header = FALSE, sep = ',')
ETFCzoo <- read.zoo(ETFCtxt, sep = ",", format = "%m/%d/%Y", split = 3)
ETFCxts <- as.xts(ETFCzoo)

TDA.x <- na.omit(cbind(ETFCxts[,c(9,20:28)]))
Schwab.x <- na.omit(cbind(ETFCxts[,c(10:17,29)]))
Fidelity.x <- na.omit(cbind(ETFCxts[,c(1:8,18)]))

#Calculate daily log component returns
TDA.Ret <- na.omit(Return.calculate(TDA.x, method = ("discrete")))
Schwab.Ret <- na.omit(Return.calculate(Schwab.x, method = ("discrete")))
Fidelity.Ret <- na.omit(Return.calculate(Fidelity.x, method = ("discrete")))

#Extract Returns by asset class
TDAEQ.Ret <-na.omit(cbind(TDA.Ret[,c(1,3,4,7,8,10)]))
TDAFI.Ret <- cbind(TDA.Ret[,c(2,5,6,9)])

SchEQ.Ret <- cbind(Schwab.Ret[,c(1:5,7)])
SchFI.Ret <- cbind(Schwab.Ret[,c(6,8:9)])

FidEQ.Ret <- cbind(Fidelity.Ret[,c(2,3,4,5,6,7)])
FidFI.Ret <- cbind(Fidelity.Ret[,c(1,8,9)])

#Create Portfolio Spec for modeling mean-variance portfolio
Default <- portfolioSpec()
setAlpha(Default) <- .05 #Sets Confidence Interval for CVaR .01 = 99%, .05 =95%

TDAFIroll <- rollingWindows(TDAFI.Ret, period = "12m", by = "1m")
SCHFIroll <- rollingWindows(SchFI.Ret, period = "12m", by = "1m")
FIDFIroll <- rollingWindows(FidFI.Ret, period = "12m", by = "1m")

TDAEQroll <- rollingWindows(TDAEQ.Ret, period = "12m", by = "1m")
SCHEQroll <- rollingWindows(SchEQ.Ret, period = "12m", by = "1m")
FIDEQroll <- rollingWindows(FidEQ.Ret, period = "12m", by = "1m")

#Create a Mean-Variance Optimized Portfolio for Highest Sharpe Ratio Using Rolling Windows#
TDAFI.roll <- rollingTangencyPortfolio(as.timeSeries(TDAFI.Ret), spec = Default,
                                       constraints = c('minW[1:3] = .1', 'maxW[1:3] = .5'),
                                       from = TDAFIroll$from, to = TDAFIroll$to)

SchFI.roll <- rollingTangencyPortfolio(as.timeSeries(SchFI.Ret), spec = Default,
                                       constraints = c('minW[1:3] = .1', 'maxW[1:3] = .5'),
                                       from = SCHFIroll$from, to = SCHFIroll$to)

FIDFI.roll <- rollingTangencyPortfolio(as.timeSeries(FidFI.Ret), spec = Default,
                                       constraints = c('minW[1:3] = .1', 'maxW[1:3] = .5'),
                                       from = FIDFIroll$from, to = FIDFIroll$to)

### Constraints ###
# Minimum Weight to International and Domestic Equity 5%
# Maximum Weight to International and EEM (Group) 25%
# Maximum Weight to Domestic Equities 80%
# Maximum Weight to REITs 10%

TDAEQ.roll <- rollingTangencyPortfolio(as.timeSeries(TDAEQ.Ret), spec = Default,
                                        constraints = c('minW[2:6] = .05', 'maxW[1] = .10', 
                                                        'maxsumW[4:6] = .80','maxsumW[2:3] = .25'),
                                       from = TDAEQroll$from, to = TDAEQroll$to)

SchEQ.roll <- rollingTangencyPortfolio(as.timeSeries(SchEQ.Ret), spec = Default,
                                       constraints = c('minW[c(1,2,3,5,6)] = .05', 'maxW[4] = .10',
                                                       'maxsumW[c(1,5,6)] = .80', 'maxsumW[2:3] = .25'),
                                       from = SCHEQroll$from, to = SCHEQroll$to)

FIDEQ.roll <- rollingTangencyPortfolio(as.timeSeries(FidEQ.Ret), spec = Default,
                                       constraints = c('minW[1:5] = .05', 'maxW[6] = .10',
                                                       'maxsumW[c(3,4,5)] = .90', 'maxsumW[1:2] = .25'),
                                       from = FIDEQroll$from, to = FIDEQroll$to)


print(last(Equity.roll))
print(last(Fixed.roll))
print(last(SchEQ.roll))
print(last(SchFI.roll))
