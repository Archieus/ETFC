library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = 'UTC')

#### Close data from TC2000 Convert TXT to XTS ####
TRtxt <- read.table('tdetfc', header = FALSE, sep = ",")

TRzoo <- read.zoo(TRtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TRxts <- as.xts(TRzoo)
TRxts <- TRxts['2014-11-07:/']

Asset.Ret <- na.omit(Return.calculate(TRxts, method = ("discrete")))

NVWeights <- read.csv("ExpNaiveWts.csv")
NVWeights.z <- read.zoo("ExpNaiveWts.csv", header = TRUE, sep = ",")
NVWeights.x <- as.xts(NVWeights.z)

PortRet.ep <- endpoints(Asset.Ret, on = 'months', k=1) 
PortRet.mo <- Asset.Ret[PortRet.ep]

NVPORTRet <- round(Return.portfolio(PortRet.mo, weights = NVWeights.x, wealth.index = FALSE, 
                                    contribution = FALSE, geometric = TRUE), digits = 6)

#### 60/40 Exp Reg MVO ####

Wts60E <- read.csv("6040All.csv")
Wts60E.z <- read.zoo("6040All.csv", header = TRUE, sep = ",")
Wts60E.x <- as.xts(Wts60E.z)

SixtyRet.ep <- endpoints(Asset.Ret, on = 'months', k=1) 
SixtyRet.mo <- Asset.Ret[SixtyRet.ep]

SixtyPORTRet <- round(Return.portfolio(PortRet.mo, weights = Wts60E.x, wealth.index = FALSE, 
                                    contribution = FALSE, geometric = TRUE), digits = 6)

#### 70/30 Exp Reg MVO ####

Wts70E <- read.csv("7030All.csv")
Wts70E.z <- read.zoo("7030All.csv", header = TRUE, sep = ",")
Wts70E.x <- as.xts(Wts70E.z)

SevntyRet.ep <- endpoints(Asset.Ret, on = 'months', k=1) 
SevntyRet.mo <- Asset.Ret[SevntyRet.ep]

SevntyPORTRet <- round(Return.portfolio(PortRet.mo, weights = Wts70E.x, wealth.index = FALSE, 
                                       contribution = FALSE, geometric = TRUE), digits = 6)

#### 90/30 Exp Reg MVO ####

Wts90E <- read.csv("9010All.csv")
Wts90E.z <- read.zoo("9010All.csv", header = TRUE, sep = ",")
Wts90E.x <- as.xts(Wts90E.z)

NinetyRet.ep <- endpoints(Asset.Ret, on = 'months', k=1) 
NinetyRet.mo <- Asset.Ret[NinetyRet.ep]

NinetyPORTRet <- round(Return.portfolio(PortRet.mo, weights = Wts90E.x, wealth.index = FALSE, 
                                        contribution = FALSE, geometric = TRUE), digits = 6)

table.CalendarReturns(NVPORTRet)
table.CalendarReturns(SixtyPORTRet)
table.CalendarReturns(SevntyPORTRet)
table.CalendarReturns(NinetyPORTRet)

SharpeRatio(NVPORTRet['2016'], .01/12, FUN = "StdDev")
SharpeRatio(SixtyPORTRet['2016'], .01/12, FUN = "StdDev")
SharpeRatio(SevntyPORTRet['2016'], .01/12, FUN = "StdDev")
SharpeRatio(NinetyPORTRet['2016'], .01/12, FUN = "StdDev")

