library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = 'UTC')

#### Close data from TC2000 Convert TXT to XTS ####
TDtxt <- read.table('spdrvang', header = FALSE, sep = ",")

TDzoo <- read.zoo(TDtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TDxts <- as.xts(TDzoo)
TDxts <- TDxts['2014-11-07:/']

Asset.Ret <- na.omit(Return.calculate(TDxts, method = ("discrete")))

EQandRA <- cbind(Asset.Ret[,c(4,5,7,8,11,12,14:19)])
FixedInc <- cbind(Asset.Ret[,c(1:3,6,9,10,13)])

chart.Correlation(EQandRA,EQandRA)
chart.Correlation(FixedInc, FixedInc)

sd(Asset.Ret$SPLG)
mean(Asset.Ret$SPLG)

sd(Asset.Ret$IWB)
mean(Asset.Ret$IWB)

EQRA.mu <- lapply(EQandRA, function(x) {mean(x)})
EQRA.sd <- lapply(EQandRA, function(x) {sd(x)})
EQSharpe <- lapply(EQandRA, function(x) {SharpeRatio(x, .01/12, FUN = "StdDev")})

EQ.Comp <- cbind(EQRA.mu,EQRA.sd, EQSharpe)

FINC.mu <- lapply(FixedInc, function(x) {mean(x)})
FINC.sd <- lapply(FixedInc, function(x) {sd(x)})
FISharpe <- lapply(FixedInc, function(x) {SharpeRatio(x, .01/12, FUN = "StdDev")})

FI.Comp <- cbind(FINC.mu,FINC.sd, FISharpe)

write.csv(EQ.Comp, file = "Equity Comparison.csv")
write.csv(FI.Comp, file = "Fixed Inc Comp.csv")
