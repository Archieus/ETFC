library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = 'UTC')

#### Close data from TC2000 Convert TXT to XTS ####
TRtxt <- read.table('coresectortda', header = FALSE, sep = ",")

TRzoo <- read.zoo(TRtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TRxts <- as.xts(TRzoo)
TRxts <- TRxts['2014-11-07:/']

Asset.Ret <- na.omit(Return.calculate(TRxts, method = ("discrete")))
Equity.Ret <- cbind(Asset.Ret[,c(1:14,18)])


CPWeights <- read.csv("corewts.csv")
CPWeights.z <- read.zoo("corewts.csv", header = TRUE, sep = ",")
CPWeights.x <- as.xts(CPWeights.z)

PortRet.ep <- endpoints(Asset.Ret, on = 'months', k=1) 
PortRet.mo <- Asset.Ret[PortRet.ep]

CPPORTRet <- round(Return.portfolio(PortRet.mo, weights = CPWeights.x, wealth.index = FALSE, 
                                    contribution = FALSE, geometric = TRUE), digits = 6)


EQWeights <- read.csv("equitywts.csv")
EQWeights.z <- read.zoo("equitywts.csv", header = TRUE, sep = ",")
EQWeights.x <- as.xts(EQWeights.z)

EquityRet.ep <- endpoints(Equity.Ret, on = 'months', k=1) 
EquityRet.mo <- Equity.Ret[EquityRet.ep]

EQPORTRet <- round(Return.portfolio(EquityRet.mo, weights = EQWeights.x, wealth.index = FALSE, 
                                    contribution = FALSE, geometric = TRUE), digits = 6)

